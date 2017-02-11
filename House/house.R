library(tidyverse)
library(stringr)

# Load the data
deputies <- read_csv("House/Data/dirty_deputies_v2.csv")

# Get count of refund descriptions
deputies %>%
  ggplot() +
    geom_bar(aes(refund_description)) +
    coord_flip() +
    theme_minimal()
