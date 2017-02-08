library(tidyverse)
library(stringr)
library(maps)
library(threejs)
library(plotrix)

# Functions
clean_names <- function(names) {
  names <- str_replace_all(names, "_", " ")
  names <- str_replace_all(names, "\\([a-zA-Z]+\\)", "")
  names[str_detect(names, "Korea")] <- "South Korea"
  
  return(names)
}
match_country <- function(regex, names) {
  matches <- c()
  for (i in 1:length(regex)) {
    match <- grep(regex[i], names, ignore.case = TRUE, perl = TRUE)
    matches[i] <- ifelse(length(match) == 0, NA, names[match])
  }
  
  return(matches)
}

# Load data
countries <- read_csv("Japan/Data/country_eng.csv")
trades <- read_csv("Japan/Data/ym_latest.csv")

# Make names of columns consistent
names(countries) <- str_replace_all(str_to_lower(names(countries)), " ", "_")
names(trades) <- str_replace_all(str_to_lower(names(trades)), " ", "_")

# Join tables and keep only relevant columns
totals <- trades %>%
  left_join(countries) %>%
  select(country_name, value) %>%
  group_by(country_name) %>%
  summarise(value = sum(as.numeric(value))) %>%
  mutate(value = rescale(value, c(0.3, 8)))

# Get coordinates for capital of each country
capitals <- world.cities %>%
  filter(capital == 1) %>%
  transmute(
    country_name = country.etc,
    lat = lat,
    long = long
  ) %>%
  rbind(c("Hong Kong", 22.39, 114.1))

# Get a correspondence between country names in 'totals' and 'capitals'
totals$country_name <- clean_names(totals$country_name)
regex <- countrycode::countrycode_data$country.name.en.regex
country_names <- as_tibble(
  cbind(
    regex,
    country_totals = match_country(regex, totals$country_name),
    country_capitals = match_country(regex, capitals$country_name)
  )
)

# Create table with all the information we need
geo_trades <- totals %>%
  left_join(country_names[,2:3], by = c("country_name" = "country_totals")) %>%
  left_join(capitals, by = c("country_capitals" = "country_name")) %>%
  filter(!is.na(country_capitals)) %>%
  select(-country_capitals)

# Create origins and destinations for arcs (origin is always Tokio)
arcs <- geo_trades %>%
  as_tibble() %>%
  cbind(
    origin_lat = 35.68,
    origin_long = 139.69
  ) %>%
  select(-country_name) %>%
  transmute(
    origin_lat = origin_lat,
    origin_long = origin_long,
    dest_lat = lat,
    dest_long = long,
    value = value
  )

# Get the image of the globe
earth <- system.file("images/world.jpg",  package = "threejs")

# Create empty globe
globejs(
  img = earth, lat = arcs$dest_lat, long = arcs$dest_long, arcs = arcs[, 1:4],
  arcsOpacity = 0.6, arcsHeight = 0.5, arcsLwd = arcs$value,
  arcsColor = "green", atmosphere = TRUE, height = 1200,
  width = 1200, bg = "white", value = 4
)


