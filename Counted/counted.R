library(tidyverse)
library(lubridate)
library(animation)
library(stringr)
library(ggrepel)
library(maptools)
library(ggmap)

# Read the data (and bind them)
deaths <- rbind(read_csv("Counted/2015.csv"), read_csv("Counted/2016.csv"))

# Initial treatment of the dataset
deaths <- deaths %>%
  mutate(date = ymd(paste0(year, month, day))) %>%
  select(-name, -streetaddress, -month, -day, -year) %>%
  arrange(date)

# Filter deaths to show only continental US
cont_deaths <- deaths %>%
  filter(state != "HI", state != "AK")

# Get coordinates of major US cities (more than 300k inhabitants)
major_cities <- us.cities %>%
  filter(country.etc != "HI", country.etc != "AK", pop > 300000) %>%
  mutate(name = str_replace(name, " [A-Z][A-Z]", ""))

# Get coordinates of most deadly US cities (more than 5 deaths in 2015-16)
deadly_cities <- us.cities %>%
  mutate(city = str_replace(name, " [A-Z][A-Z]", "")) %>%
  inner_join(cont_deaths, c("city" = "city", "country.etc" = "state")) %>%
  group_by(city, country.etc) %>%
  summarise(count = n(), long = long[1], lat = lat[1]) %>%
  ungroup() %>%
  filter(count > 5)

# Create base layer of the plot (continental US)
plot_deaths <- ggplot() + 
  geom_polygon(data = map_data("usa"), aes(long, lat, group = group), fill = "#e6e6e6") + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none") + 
  coord_quickmap()

# Plot deaths and show major cities
plot_deaths +
  geom_text_repel(data = major_cities, aes(long, lat, label = name), size = 4) +
  geom_point(data = cont_deaths, aes(longitude, latitude), alpha = 0.2, color = "red") +
  ggtitle("Killed by Police (showing major US cities)")

# Plot deaths and show most deadly cities
plot_deaths +
  geom_text_repel(data = deadly_cities, aes(long, lat, label = city), size = 4) +
  geom_point(data = cont_deaths, aes(longitude, latitude), alpha = 0.2, color = "red") +
  ggtitle("Killed by Police (showing cities with most deaths)")

# Create GIF with the 730 days represented by the data
saveGIF(for (i in 0:730) {
  
  # Filter deaths up to a certain date
  time_deaths <- cont_deaths %>%
    filter(date <= ymd("2015-01-01") + i)
  
  # Get the cities that have already had more than 5 deaths
  time_cities <- deadly_cities %>%
    left_join(time_deaths, c("city" = "city", "country.etc" = "state")) %>%
    group_by(city, country.etc) %>%
    summarise(count = n(), long = long[1], lat = lat[1]) %>%
    ungroup() %>%
    mutate(alph = count > 5)
  
  # Plot deaths
  print(plot_deaths +
    geom_text_repel(data = time_cities, size = 4, segment.alpha = 0, 
                    aes(long, lat, label = city, alpha = factor(alph))) +
    scale_alpha_manual(values = c(0, 1)) +
    geom_point(data = time_deaths, aes(longitude, latitude), alpha = 0.2, color = "red") +
    ggtitle(paste0("Deaths until ", ymd("2015-01-01") + i,
                   " (showing when each city crosses the 5 deaths line)")))
  
}, "deaths.gif", interval = 0.005, ani.width = 700, ani.height = 490)

