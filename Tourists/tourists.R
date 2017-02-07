library(tidyverse)
library(stringr)
library(ggplot2)
library(imputeTS)
library(forecast)



# FUNCTIONS -------------------------------------------------------------------

# Clean 'Month' variable of the 'tourists' dataset
clean_month <- function(Month) {

  # Create factor levels
  month_levels <- c(
    "jan", "fev", "mar", "abr", "mai", "jun",
    "jul", "ago", "set", "out", "nov", "dez"
  )

  # Change encoding, trim names and apply factor levels
  Month %>%
    iconv(from = "latin1", to = "UTF-8") %>%
    str_sub(1, 3) %>%
    factor(levels = month_levels)
}

# Clean 'State' variable of the 'tourists' dataset
clean_state <- function(State) {

  # Change encoding
  State %>%
    iconv(from = "latin1", to = "UTF-8")
}

# Fill count of tourists with moving average
fill_count <- function(X) {
  X$Count <- na.ma(X$Count)
  X
}



# CODE ------------------------------------------------------------------------

# Read data
tourists <- read_csv("Tourists/Data/touristData.csv")

# Clean problematic variables
tourists <- tourists %>%
  mutate(
    Month = clean_month(Month),
    State = clean_state(State)
  )

# Fill gaps in 'Count' column for all states
tourists_filled <- tourists %>%
  group_by(State, Year, Month) %>%
  summarise(Count = sum(Count)) %>%
  group_by(State, Month) %>%
  nest() %>%
  mutate(data = map(data, fill_count)) %>%
  unnest(data)

# Plot visits to 'State' over time
tourists_filled %>%
  group_by(State) %>%
  arrange(Year, Month) %>%
  mutate(Time = row_number()) %>%
  ggplot(aes(Time, Count, color = State)) +
    geom_smooth(se = FALSE) +
    theme_minimal()

# Plot total visits over time (by month)
tourists_filled %>%
  group_by(Year, Month) %>%
  summarise(Count = sum(Count)) %>%
  ungroup() %>%
  mutate(Time = row_number()) %>%
  ggplot(aes(Time, Count, color = Month)) +
    geom_point() +
    theme_minimal()

# Plot time series
tourists_filled %>%
  group_by(Year, Month) %>%
  summarise(Count = sum(Count)) %>%
  ungroup() %>%
  mutate(Time = row_number()) %>%
  ggplot(aes(Time, Count)) +
    geom_line() +
    theme_minimal()

# Create table with time series only
tourists_ts <- tourists_filled %>%
  group_by(Year, Month) %>%
  summarise(Count = sum(Count)) %>%
  ungroup()

# Forecast next five months of the time series with ARIMA
ts <- ts(tourists_ts$Count, c(1989, 1), c(2015, 12), 12)
fit <- Arima(ts, order = c(1, 0, 12))

# Plot predictions
tourists_ts %>%
  bind_rows(tibble(
    Year = rep(2016, 6),
    Month = c("jan", "fev", "mar", "apr", "mai", "jun"),
    Count = forecast(fit, 6)$mean[1:6]
  )) %>%
  mutate(Time = row_number()) %>%
  ggplot(aes(Time, Count)) +
    geom_line() +
    theme_minimal()
