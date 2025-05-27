# STDA Final Project
# Wheelchair-accesible Taxies in Seoul EDA
# 21900749
# Seungri Choe

### Library ###
library(readr)
library(readxl)
library(glue)
library(forecast)
library(fpp3)
library(sf)
library(stringr)
library(timeDate)
library(gridExtra)


### Data Load ###
setwd('')

# Taxi 
df <- read.csv('complete_taxi_data.csv', fileEncoding = "CP949")
df <- df |>
  mutate(Date = ymd(Date)) |>
  as_tsibble(index = Date)


### Trend ###
df_trend <- df |>
  mutate(
    `365-MA` = slider::slide_dbl(count, mean,
                                 .before = 182, .after=182, .complete = TRUE)
  )
df_trend |>
  autoplot(count) + 
  labs(title = "Daily Demand of Wheelchair-Accessible Taxi in Seoul ")

### COVID ###
par(mfrow = c(3,1))

count_plot <- df |> select(Date, count) |>
  filter_index("2020"~"2022") |>
  autoplot(count) +
  labs(title = "Daily Demand of Wheelchair-Accessible Taxi in Seoul", y = "Demand")

case_plot <- df |> select(Date, covid_case) |>
  filter_index("2020"~"2022") |>
  autoplot(covid_case) +
  labs(title = "Daily New Confirmed Cases", y = "Cases")

trend_plot <- df |> select(Date, covid_trend) |>
  filter_index("2020"~"2022") |>
  autoplot(covid_trend) +
  labs(title = "Search Frequency for 코로나", y = "Frequency")

covid_trend_plot <- 
  grid.arrange(count_plot, case_plot, trend_plot, nrow = 3)

### Weekly Patterns ###
df_weekly <- df
df_weekly$weekday <- wday(df_weekly$Date, label = TRUE, abbr = TRUE)
df_weekly_sum <- df_weekly |> as_tibble() |>
  group_by(weekday) |>
  summarize(Count = mean(count))
ggplot(df_weekly_sum, aes(x = weekday, y = Count)) +
  geom_col(fill = "skyblue", color = "black") +  # Bar color and outline
  labs(
    title = "Weekday Count Histogram",
    x = "Weekday",
    y = "Count"
  ) +
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    text = element_text(size = 12),
    axis.title = element_text(face = "bold")
  )

### Yearly Patterns ###
df_yearly <- df
df_yearly <- df |>
  mutate(month = yearmonth(Date)) |>
  as_tibble() |>
  select(month, count) |>
  group_by(month) |>
  summarize(count = mean(count)) |>
  as_tsibble(index = month)

df_yearly  |> gg_subseries()
df_yearly |> gg_season()
df_yearly |> filter(year(month)==2020) |> gg_season()

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

WorkingDays <-mean((df |> filter(weekends == FALSE & PureHoliday == FALSE))$count)
HoliDays <- mean((df |> filter(PureHoliday == TRUE))$count)

wh_comparison <- data.frame(
  Type = c("WorkingDays", "HoliDays"),
  Value = c(WorkingDays, HoliDays)
)

ggplot(wh_comparison, aes(x=Type, y=Value)) +
  geom_col(fill = "skyblue", color = "black") +  # Bar color and outline
  labs(
    title = "Weekday Count Histogram",
    x = "Weekday",
    y = "Count"
  ) +
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    text = element_text(size = 12),
    axis.title = element_text(face = "bold")
  )

