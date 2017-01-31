library(tidyverse)
library(stringr)
library(forcats)
library(ggplot2)
library(imputeTS)



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

# Fill gaps in time series of a State with moving average
fill_tot <- function(X) {
  X$Tot <- na.ma(X$Tot)
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

# Fill gaps in 'Tot' column for all states
tourists_filled <- tourists %>%
  group_by(State, Year, Month) %>%
  summarise(Tot = sum(Count)) %>%
  group_by(State, Month) %>%
  nest() %>%
  mutate(data = map(data, fill_tot)) %>%
  unnest(data)

# Create plot of visits to 'State' over time
tourists_filled %>%
  group_by(State, Year, Month) %>%
  ggplot(aes(Year, Tot, color = State)) +
    geom_smooth(se = FALSE) +
    theme_minimal()

# tourists %>%
#   group_by(State, Year, Month) %>%
#   summarise(Tot = sum(Count)) %>%
#   ggplot(aes(Year, Tot, color = State)) +
#   geom_smooth(se = FALSE)








