# STDA Final Project
# Wheelchair-accesible Taxies in Seoul Model
# 21900749
# Seungri Choe


###############
### Library ###
###############
library(readr)
library(readxl)
library(glue)
library(forecast)
library(fpp3)
library(sf)
library(stringr)
library(timeDate)
library(gridExtra)
library(splines)



#############
### Data  ###
#############
#setwd('')

# Taxi 
df <- read.csv('complete_taxi_data.csv', fileEncoding = "CP949")
df <- df |>
  mutate(Date = ymd(Date)) |>
  as_tsibble(index = Date)

### Holiday ###
# Weekends
df <- df |>
  mutate(weekends = isHoliday(timeDate(Date), holidays = "Korea"))

# Holiday
raw_holiday <- read_xlsx('holiday_data_2009_2024.xlsx')
raw_holiday <- raw_holiday |>
  mutate(locdate = ymd(locdate))
df <- df |>
  mutate(
    holiday = Date %in% raw_holiday$locdate
  )
df <- df |>
  mutate(PureHoliday = (df$weekends == FALSE & df$holiday == TRUE))

# Train test split
df_train <- df |>
  filter_index(.~"2022")

df_test <- df |>
  filter_index("2023"~.)

df_test

##################
# Transformation #
##################
df |>
  autoplot(log(count)) +
  labs(title = "Daily Demand of Wheelchair-Accessible Taxi in Seoul ")



#################
# Initial Check #
#################
## Initial Model
# Decomposition
mstl_decomposition <- mstl(log(df$count), seasonal.periods = c(7, 365.25))
trend_component <- mstl_decomposition[,'Trend']
trend_component |> autoplot()
rest_component <- mstl_decomposition[,'Remainder']
rest_component |> autoplot()

# Trend model
trend_model <- ets(trend_component, model = "ANN")
trend_model |> forecast(h=600) |>
  autoplot()

# Model
initial_model <- df |>
  model(
    ARIMA = ARIMA(log(count) ~
                    trend_component +
                    PureHoliday + 
                    fourier(period = 365.25, K=10)+
                    PDQ(D=1) + pdq(d=0) +
                    covid_trend)
  )

# Plot
initial_model |> gg_tsresiduals()
report(initial_model)

# Unusual days
unusual_days <- augment(initial_model) |> filter(.innov < -0.4) |> select(Date)

# Update df
df_new <- df |> mutate(unusual = (df$Date %in% ymd(unusual_days$Date))) |>
  mutate(PureUnusualHoliday = (weekends == FALSE & (holiday == TRUE | unusual == TRUE)))
#write.csv(df_new, file = 'complete_taxi_data_new.csv', row.names = FALSE)


############
# Modeling #
############
# Import data
#setwd('C:/Users/Victor/Desktop/한동대 수업 (2024-2)/시공간데이터분석/STDA_TermProject/Data')
df <- read.csv('complete_taxi_data_new.csv')
df <- df |>
  mutate(Date = ymd(Date)) |>
  as_tsibble(index = Date)

### Hyperparameter Tuning
covid_model <- df |>
  model(
    lagno = ARIMA(log(count) ~
                    trend_component +
                    PureUnusualHoliday + 
                    fourier(period = 365.25, K=10)+
                    PDQ(D=1) + pdq(d=0)),
    lag0 = ARIMA(log(count) ~
                    trend_component +
                    PureUnusualHoliday + 
                    fourier(period = 365.25, K=10)+
                    PDQ(D=1) + pdq(d=0) +
                    covid_trend),
    lag1 = ARIMA(log(count) ~
                    trend_component +
                    PureUnusualHoliday + 
                    fourier(period = 365.25, K=10)+
                    PDQ(D=1) + pdq(d=0) +
                    covid_trend +
                    lag(covid_trend)),
    lag2 = ARIMA(log(count) ~
                    trend_component +
                    PureUnusualHoliday + 
                    fourier(period = 365.25, K=10)+
                    PDQ(D=1) + pdq(d=0) +
                    covid_trend +
                    lag(covid_trend)  +
                    lag(covid_trend, 2)),
    lag3 = ARIMA(log(count) ~
                    trend_component +
                    PureUnusualHoliday + 
                    fourier(period = 365.25, K=10)+
                    PDQ(D=1) + pdq(d=0) +
                    covid_trend +
                    lag(covid_trend)  +
                    lag(covid_trend, 2) +
                    lag(covid_trend, 3)),
    lag4 = ARIMA(log(count) ~
                   trend_component +
                   PureUnusualHoliday + 
                   fourier(period = 365.25, K=10)+
                   PDQ(D=1) + pdq(d=0) +
                   covid_trend +
                   lag(covid_trend)  +
                   lag(covid_trend, 2) +
                   lag(covid_trend, 3) +
                   lag(covid_trend, 4))
  )

report(covid_model |> select(lag0))
covid_model |>
  select(lag0) |>
  gg_tsresiduals()

### Evaluation
# train/test split
df_train <- df |>
  filter_index(.~"2022")
df_test <- df |>
  filter_index("2023"~.)

# train the model
eval_model <- df_train |>
  model(
    ARIMA = ARIMA(log(count) ~
                    trend_component +
                    PureUnusualHoliday + 
                    fourier(period = 365.25, K=10)+
                    PDQ(D=1) + pdq(d=0) +
                    covid_trend))

# Forecast and Test
trend_model <- ets(df_train$trend_component, model = "ANN")
trend_forcasted <- trend_model |> forecast(h=670)
trend_forcasted$mean |> autoplot()

df_future <- new_data(df_train, 670) |>
  mutate(trend_component = trend_forcasted$mean,
         PureUnusualHoliday = df_test$PureUnusualHoliday,
         covid_trend = df_test$covid_trend)

forecasts <- eval_model |>
  forecast(df_future)

accuracy(forecasts, df_test)


# Plot
# Define the desired date range
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2024-10-31")

# Filter the forecasted data
filtered_forecasts <- forecasts %>%
  filter(Date >= start_date & Date <= end_date)

# Filter the actual data
filtered_actuals <- df_test %>%
  filter(Date >= start_date & Date <= end_date)

# Plot the forecasts and actual data for the specified period
autoplot(filtered_forecasts, level = FALSE) +
  autolayer(filtered_actuals, series = "Actuals") +
  labs(title = "Forecast vs Actuals",
       x = "Date",
       y = "Value") +
  theme_minimal()

# Accuracy
accuracy(forecasts, df_test)



