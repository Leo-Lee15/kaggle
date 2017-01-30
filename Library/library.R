library(raster)
library(tidyverse)
library(ggmap)
library(animation)
library(stringr)
library(lubridate)
library(rgdal)

# Read the files
usage <- read_csv("Library/Library_Usage.csv")
shape <- shapefile("Library/Supervisor Districts as of April 2012/geo_export_38fd3153-0303-488e-9f3c-0f81e8e00115.shp")

# Converts the names of the columns to snake case
names(usage) <- str_to_lower(str_replace_all(names(usage), " ", "_"))

# Create a table of mean yearly checkouts and renewals per
# district and resgistration year
dstr_usage <- usage %>%
  filter(
    !is.na(supervisor_district),
    circulation_active_year != "None"
  ) %>%
  mutate(
    active_years = as.numeric(circulation_active_year) -
      as.numeric(year_patron_registered) + 1,
    yearly_checkouts = total_checkouts/active_years,
    yearly_renewals = total_renewals/active_years
  ) %>%
  group_by(
    supervisor_district,
    year_patron_registered
  ) %>%
  summarise(
    mean_yearly_checkouts = mean(yearly_checkouts),
    mean_yearly_renewals = mean(yearly_renewals)
  )

# Transform shape into a collection of points
points <- fortify(shape, region = 'supervisor')

# Create base plot for the bay area
bay <- qmap("San Francisco, CA", zoom = 12, maptype = "watercolor")

# Create GIF with the 14 years represented by the data
saveGIF(for (i in 0:13) {
  
  # Filter only one year of usage data
  dstr_usage_ <- dstr_usage %>%
    filter(
      year_patron_registered == 2003 + i
    )
  
  # Merge outlines of districts and usage information
  geo_usage <- merge(points, dstr_usage_, by.x = 'id', by.y = 'supervisor_district', all.x = TRUE)
  
  # Create the plot of the neighborhoods
  print(bay +
    geom_polygon(aes(x = long, y = lat, group = group, fill = mean_yearly_checkouts),
                 data = geo_usage, linetype = "dotted", color = "#0f0f0f", alpha = 0.55) +
    scale_fill_gradient(low = 'red', high = 'green', limits = c(10, 54)) +
    ggtitle(paste0("Average checkouts per person per year for patrons registered in ", 2003 + i)))
  
}, "checkouts.gif", interval = 0.5, ani.width = 1000, ani.height = 1000)

