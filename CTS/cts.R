library(tidyverse)

# Read data
cts <- read_csv("CTS/Data/cts.csv")

# Get Trump Score for senators
cts %>%
  filter(chamber == "Senate", party != "I") %>%
  ggplot() +
    scale_colour_manual(values = c("blue", "red")) +
    geom_point(aes(trump_margin, trump_score, color = party)) +
    geom_smooth(aes(trump_margin, trump_score, color = party), method = "lm", se = FALSE) +
    ggtitle("Fa·vor·it·ism, n.", "How often senators vote in line with Trump as a function of Trumps's margin in their states") +
    xlab("Trump's Margin") +
    ylab("Trump Score") +
    theme_minimal()
