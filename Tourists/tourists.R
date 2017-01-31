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



# CODE ------------------------------------------------------------------------

# Read data
tourists <- read_csv("Tourists/Data/touristData.csv")

# Clean problematic variables
tourists <- tourists %>%
  mutate(
    Month = clean_month(Month),
    State = clean_state(State)
  )

# Create variable with total visits to 'State' over time
tourists %>%
  group_by(State, Year, Month) %>%
  summarise(Tot = sum(Count)) %>%
  ggplot(aes(Year, Tot, color = State)) +
    geom_smooth(se = FALSE)

s <- tourists %>%
  filter(State == "São Paulo") %>%
  group_by(Year, Month) %>%
  arrange(Year, Month) %>%
  summarise(Tot = sum(Count)) %>%
  ungroup() %>%
  mutate(Time = row_number())

s %>%
  ggplot(aes(Time, Tot)) +
    geom_point() +
    geom_smooth(se = FALSE)

s %>%
  group_by(Month) %>%
  arrange(Time) %>%
  mutate(Tot = na.ma(Tot)) %>%
  ungroup() %>%
  ggplot(aes(Time, Tot)) +
    geom_point() +
    geom_smooth(se = FALSE)










