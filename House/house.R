library(highcharter)
library(tidyverse)



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
    cnt_refund = n(),
    avg_refund = mean(refund_value),
    tot_refund = sum(refund_value),
    state = deputy_state[1],
    party = political_party[1],
    ideology = party_ideology1[1]
  )



## DISTRIBUTION PLOTS ---------------------------------------------------------

# Plot times reimbursed
deputies_summ %>%
  arrange(cnt_refund) %>%
  mutate(deputy_name = reorder_ftc(deputy_name)) %>%
  ggplot(aes(deputy_name, cnt_refund, color = party)) +
    geom_point() +
    my_theme + 
    guides(col = guide_legend(ncol = 20))

# Plot mean amount reimbursed
deputies_summ %>%
  arrange(avg_refund) %>%
  mutate(deputy_name = reorder_ftc(deputy_name)) %>%
  ggplot(aes(deputy_name, avg_refund, color = party)) +
    geom_point() +
    my_theme + 
    guides(col = guide_legend(ncol = 20))

# Plot total amount reimbursed
deputies_summ %>%
  arrange(tot_refund) %>%
  mutate(deputy_name = reorder_ftc(deputy_name)) %>%
  ggplot(aes(deputy_name, tot_refund, color = party)) +
    geom_point() +
    my_theme + 
    guides(col = guide_legend(ncol = 20))



## DENSITY PLOTS --------------------------------------------------------------

# Plot times reimbursed
hchart(
  density(deputies_summ$cnt_refund, from = 0),
  type = "area",
  color = "#B71C1C",
  name = "Refund Count"
)

# Plot mean amount reimbursed
hchart(
  density(deputies_summ$avg_refund),
  type = "area",
  color = "#B71C1C",
  name = "Mean Refund Value"
)

# Plot total amount reimbursed
hchart(
  density(deputies_summ$tot_refund),
  type = "area",
  color = "#B71C1C",
  name = "Total Refund Value"
)



## CORRELATION PLOTS ----------------------------------------------------------

# Plot times reimbursed X mean amount reimbursed
deputies_summ %>%
  arrange(cnt_refund) %>%
  mutate(deputy_name = reorder_ftc(deputy_name)) %>%
  ggplot(aes(cnt_refund, avg_refund, color = party)) +
    geom_point() +
    theme_classic() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom"
    ) +
    guides(col = guide_legend(ncol = 20))

# Plot times reimbursed X total amount reimbursed
deputies_summ %>%
  arrange(cnt_refund) %>%
  mutate(deputy_name = reorder_ftc(deputy_name)) %>%
  ggplot(aes(cnt_refund, tot_refund, color = party)) +
  geom_point() +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  guides(col = guide_legend(ncol = 20))

# Plot mean amount reimbursed X total amount reimbursed
deputies_summ %>%
  arrange(avg_refund) %>%
  mutate(deputy_name = reorder_ftc(deputy_name)) %>%
  ggplot(aes(avg_refund, tot_refund, color = party)) +
    geom_point() +
    theme_classic() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom"
    ) +
    guides(col = guide_legend(ncol = 20))
































