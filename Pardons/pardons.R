# Load packages
library(tidyverse)
library(stringr)
library(lubridate)

# Read the data
part_one <- read_csv("Pardons/presidents_one.csv")
part_two <- read_csv("Pardons/presidents_two.csv")

# Convert column names to snake case
names(part_one) <- str_replace_all(str_to_lower(names(part_one)), " ", "_")
names(part_two) <- str_replace_all(str_to_lower(names(part_two)), " ", "_")

# Remove parentheses from the column names
names(part_two) <- str_replace_all(names(part_two), "\\)", "")
names(part_two) <- str_replace_all(names(part_two), "\\(", "")

# Replace NAs with 0s
#
# I know this shouldn't be done unless we know for a fact that NAs are
# equivalent to 0s, but I felt like it was a valid decision because:
#   1. We can't remove or treat rows with NAs differently since every
#      row has at least one NA
#   2. NAs in one column seem to be related with non-NAs in another one
#      (take a look at columns 13/14 and columns 15/16 in part_two; since
#      I intended in adding these four columns anyway, it wouldn't be an
#      problem to replace the NAs with 0s)
#   3. I'm throwing out types of granted petitions (pardons, commutations,
#      etc.) anyway
#   4. As we'll see shortly, if we replace NAs with 0s the math of leftover
#      petitions still adds up pretty nicely
part_one[is.na(part_one)] <- 0
part_two[is.na(part_two)] <- 0

# Transform part_one so it fits nicely with part_two
part_one <- part_one %>%
  mutate(
    petitions_available = petitions_pending + petitions_received,
    petitions_blocked = petitions_closed_without_presidential_action +
      petitions_denied_or_closed_without_presidential_action
  ) %>%
  select(
    president,
    fiscal_year,
    petitions_pending,
    petitions_received,
    petitions_available,
    petitions_granted,
    petitions_denied,
    petitions_blocked
  )

# Transform part_two so it fits nicely with part_one
part_two <- part_two %>%
  mutate(
    petitions_denied = petitions_denied_pardons + petitions_denied_commutations,
    petitions_pending = petitions_pending_pardons + petitions_pending_commutations,
    petitions_received = petitions_received_pardons + petitions_received_commutations,
    petitions_available = petitions_pending_pardons + petitions_pending_commutations +
      petitions_received_pardons + petitions_received_commutations,
    petitions_blocked = petitions_closed_without_presidential_action_pardons +
      petitions_closed_without_presidential_action_commutations +
      petitions_denied_or_closed_without_presidential_action_pardons +
      petitions_denied_or_closed_without_presidential_action_commutations
  ) %>%
  select(
    president,
    fiscal_year,
    petitions_pending,
    petitions_received,
    petitions_available,
    petitions_granted,
    petitions_denied,
    petitions_blocked
  )

# Bind the two tables
pardons <- rbind(part_one, part_two)

# Create president ids so when we order by year we don't have to
# worry about alphabetical order
presidents <- pardons %>%
  arrange(fiscal_year) %>%
  distinct(president) %>%
  mutate(
    id = 1:20
  )

# Join president ids
pardons <- pardons %>%
  left_join(presidents) %>%
  select(id, 1:8)

# Create a variable that tells us how many petitions go missing
# (or simply show up out of nowhere) between two fiscal years
#
# As you can see, replacing NAs with 0s seems to have left us
# with very few unexplained petitions
pardons <- pardons %>%
  arrange(id, fiscal_year) %>%
  mutate(
    petitions_missing = lead(petitions_pending) -
      (petitions_available - petitions_granted -
         petitions_denied - petitions_blocked)
  )

# Save tidier dataset
write_csv(pardons, "Pardons/pardons.csv")
