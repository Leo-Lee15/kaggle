library(plotly)
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
  ) %>%
  ungroup()

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
  ) %>%
  ungroup()

# Get summary of each type of refund
desc_summ <- deputies %>%
  group_by(refund_description) %>%
  summarise(
    refund_cnt = n(),
    refund_avg = mean(refund_value),
    refund_tot = sum(refund_value)
  ) %>%
  ungroup()


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

# Plot mean amount reimbursed
t <- deputies %>%
  filter(refund_value > 10000) %>%
  select(refund_value)
t <- density(time_summ$refund_avg)
plot_ly(x = t$x, y = t$y, mode = "lines", fill = "tozeroy")



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
time_summ %>%
  plot_ly(
    x = ~refund_date,
    y = ~refund_cnt,
    type = 'scatter'
  )

# THAT'S IT
# Plot mean value of refunds
time_summ %>%
  filter(refund_date > as_date("2016-01-01")) %>%
  plot_ly(
    x = ~refund_date,
    y = ~refund_avg,
    type = 'scatter',
    mode = 'lines'
  ) %>%
  add_trace(x = c(as_date("2016-12-23")), y= c(0, 10000), mode = "lines")


# Plot total value of refunds
time_summ %>%
  filter(refund_date > as_date("2016-01-01")) %>%
  plot_ly(
    x = ~refund_date,
    y = ~refund_tot,
    type = 'scatter',
    mode = 'lines'
  )



## PARTY PLOTS ----------------------------------------------------------------

# Plot mean value of refunds by party
deputies_summ %>%
  plot_ly(
    y = ~refund_avg,
    color = ~party,
    type = "box"
  )

# Plot mean value of refunds by ideology
deputies_summ %>%
  plot_ly(
    y = ~refund_avg,
    color = ~ideology,
    type = "box"
  )

# Plot mean value of refunds by state
deputies_summ %>%
  plot_ly(
    y = ~refund_avg,
    color = ~state,
    type = "box"
  )



## BAR CHARTS -----------------------------------------------------------------

# Plot count of refunds
desc_summ %>%
  plot_ly(
    x = ~refund_description,
    y = ~refund_cnt,
    type = "bar"
  )

# Plot mean value of refunds
desc_summ %>%
  plot_ly(
    x = ~refund_description,
    y = ~refund_avg,
    type = "bar"
  )

# Plot total value of refunds
desc_summ %>%
  plot_ly(
    x = ~refund_description,
    y = ~refund_tot,
    type = "bar",
    color = ~refund_description
  ) %>%
  layout(legend = list(orientation = 'h'), xaxis = list(
    showticklabels = FALSE,
    title = ""
  ))



## DESCRIPTION PLOTS ----------------------------------------------------------

# Reorder deputies
t <- deputies %>%
  group_by(refund_description) %>%
  summarise(med = median(refund_value)) %>%
  arrange(med) %>%
  select(refund_description)
deputies$refund_description <- ordered(
  deputies$refund_description,
  levels = t$refund_description
)

# Plot refund descriptions
deputies %>%
  plot_ly(
    x = ~refund_description,
    y = ~refund_value,
    type = "box",
    color = ~refund_description
  ) %>%
  layout(legend = list(orientation = 'h'), xaxis = list(
    showticklabels = FALSE,
    title = ""
  ))

