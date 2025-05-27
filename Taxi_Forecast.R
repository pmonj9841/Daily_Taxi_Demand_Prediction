# STDA Final Project
# Wheelchair-accesible Taxies in Seoul Forecasting
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


############
### Data ###
############
df <- read.csv('complete_taxi_data_new.csv')
df <- df |>
  mutate(Date = ymd(Date)) |>
  as_tsibble(index = Date)


####################
# Real Forecasting #
####################
# Model
forecast_model <- df |>
  model(
    ARIMA = ARIMA(log(count) ~
                    trend_component +
                    PureUnusualHoliday + 
                    fourier(period = 365.25, K=10)+
                    PDQ(D=1) + pdq(d=0) +
                    covid_trend))

report(forecast_model)


### Forecast data

# PureHoliday
data_holiday <- read_xlsx("holiday_data_future.xlsx")
data_holiday <- data_holiday |>
  mutate(Holiday = ymd(Holiday))

df_future <- new_data(df, 426) |>
  mutate(weekends = isHoliday(timeDate(Date), holidays = "Korea")) |>
  mutate(holiday = Date %in% data_holiday$Holiday) |>
  mutate(PureUnusualHoliday = (weekends == FALSE & holiday == TRUE)) |>
  select(PureUnusualHoliday)


# trend
trend_model <- ets(df$trend_component, model = "ANN")
trend_forcasted <- trend_model |> forecast(h=426)
trend_forcasted$mean |> autoplot()
df_future <- df_future |>
  mutate(trend_component = trend_forcasted$mean)

# Coivd
df_future <- df_future |>
  mutate(covid_trend = 1)

# Forecast
forecasts <- forecast_model |>
  forecast(df_future)

# Plot all
autoplot(forecasts, level = FALSE) +
  autolayer(df)

# Plot all future
autoplot(forecasts, level = FALSE)

# Plot two months
# Define the desired date range
start_date <- as.Date("2024-12-01")
end_date <- as.Date("2025-01-31")

filtered_forecasts <- forecasts %>%
  filter(Date >= start_date & Date <= end_date)

# Plot the forecasts and actual data for the specified period
autoplot(filtered_forecasts, level = FALSE)



