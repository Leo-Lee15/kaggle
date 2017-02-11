library(highcharter)
library(tidyverse)
library(lubridate)



## GLOBAL ---------------------------------------------------------------------

# Reorder factors of a column
reorder_ftc <- function(x) {
  ordered(x, levels = x)
}

# Set my ggplot theme
my_theme <- theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  )



## READ & SUMMARIZE -----------------------------------------------------------

# Load the data
deputies <- read_csv("House/Data/dirty_deputies_v2.csv")

# Get the summary of each deputy
deputies_summ <- deputies %>%
  filter(!is.na(deputy_state)) %>%
  group_by(deputy_name) %>%
  summarise(
    refund_cnt = n(),
    refund_avg = mean(refund_value),
    refund_tot = sum(refund_value),
    state = deputy_state[1],
    party = political_party[1],
    ideology = party_ideology1[1]
  )

# Get summary of each day
time_summ <- deputies %>%
  filter(!is.na(deputy_state)) %>%
  filter(!is.na(refund_date)) %>%
  mutate(refund_date = as_date(refund_date)) %>%
  group_by(refund_date, wday = wday(refund_date)) %>%
  summarise(
    refund_cnt = n(),
    refund_avg = mean(refund_value),
    refund_tot = sum(refund_value)
  )


## DISTRIBUTION PLOTS ---------------------------------------------------------

# Plot times reimbursed
deputies_summ %>%
  arrange(refund_cnt) %>%
  mutate(deputy_name = reorder_ftc(deputy_name)) %>%
  ggplot(aes(deputy_name, refund_cnt, color = party)) +
    geom_point() +
    my_theme + 
    guides(col = guide_legend(ncol = 20))

# Plot mean amount reimbursed
deputies_summ %>%
  arrange(refund_avg) %>%
  mutate(deputy_name = reorder_ftc(deputy_name)) %>%
  ggplot(aes(deputy_name, refund_avg, color = party)) +
    geom_point() +
    my_theme + 
    guides(col = guide_legend(ncol = 20))

# Plot total amount reimbursed
deputies_summ %>%
  arrange(refund_tot) %>%
  mutate(deputy_name = reorder_ftc(deputy_name)) %>%
  ggplot(aes(deputy_name, refund_tot, color = party)) +
    geom_point() +
    my_theme + 
    guides(col = guide_legend(ncol = 20))



## DENSITY PLOTS --------------------------------------------------------------

# Plot times reimbursed
hchart(
  density(deputies_summ$refund_cnt, from = 0),
  type = "area",
  color = "#B71C1C",
  name = "Refund Count"
)

# Plot mean amount reimbursed
hchart(
  density(deputies_summ$refund_avg),
  type = "area",
  color = "#B71C1C",
  name = "Mean Refund Value"
)

# Plot total amount reimbursed
hchart(
  density(deputies_summ$refund_tot),
  type = "area",
  color = "#B71C1C",
  name = "Total Refund Value"
)



## CORRELATION PLOTS ----------------------------------------------------------

# Plot times reimbursed X mean amount reimbursed
deputies_summ %>%
  arrange(refund_cnt) %>%
  mutate(deputy_name = reorder_ftc(deputy_name)) %>%
  ggplot(aes(refund_cnt, refund_avg, color = party)) +
    geom_point() +
    theme_classic() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom"
    ) +
    guides(col = guide_legend(ncol = 20))

# Plot times reimbursed X total amount reimbursed
deputies_summ %>%
  arrange(refund_cnt) %>%
  mutate(deputy_name = reorder_ftc(deputy_name)) %>%
  ggplot(aes(refund_cnt, refund_tot, color = party)) +
  geom_point() +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  guides(col = guide_legend(ncol = 20))

# Plot mean amount reimbursed X total amount reimbursed
deputies_summ %>%
  arrange(refund_avg) %>%
  mutate(deputy_name = reorder_ftc(deputy_name)) %>%
  ggplot(aes(refund_avg, refund_tot, color = party)) +
    geom_point() +
    theme_classic() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom"
    ) +
    guides(col = guide_legend(ncol = 20))



## TIME SERIES ----------------------------------------------------------------

# Plot count of refunds
hchart(
  ts(time_summ$refund_cnt),
  type = "area",
  color = "#B71C1C",
  name = "Refund Count"
)

### THATS IT
# Plot mean value of refunds on weekdays
hchart(
  ts(time_summ$refund_avg),
  type = "area",
  color = "#B71C1C",
  name = "Mean Refund Value"
)

# Plot total value of refunds on weekdays
hchart(
  ts(time_summ$refund_tot),
  type = "area",
  color = "#B71C1C",
  name = "Total Refund Value"
)



























