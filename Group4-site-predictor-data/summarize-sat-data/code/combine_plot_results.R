# Combines LTER .r result files containing radii data for each site.

# written by ACS 5 Nov 2018

# load packages -----------------------------------------------------------

library(dplyr)
library(ggplot2)

# load data ---------------------------------------------------------------

# coordinate data
coords <- read.csv('/home/annie/Documents/MSU_postdoc/lter/data/LTER_coordinates.csv', 
                   header = TRUE, stringsAsFactors = FALSE)

for (i in seq(1, 11)) {
  load(paste('/home/annie/Documents/MSU_postdoc/lter/data/modis_lst_', i, '.r', sep = ''))
  stats_by_point <- stats_by_point[[1]][[1]]
  stats_by_point$site <- coords$site[i]
  stats_by_point$subsite <- coords$subsite[i]
  stats_by_point$lat <- coords$lat[i]
  stats_by_point$lon <- coords$lon[i]
  stats_by_point$area_ha <- coords$area_ha[i]
  stats_by_point <- stats_by_point %>%
    mutate(year = layer + 2000) %>%
    mutate(var = 'modis_lst_1000m')
  
  if (i == 1) {
    data <- stats_by_point
  } else {
    data <- rbind(data, stats_by_point)
  }
}

# plot to make sure everything is good ------------------------------------

# get map of state
wi_map <- map_data('state', 'wisconsin')

# summarize data by plot
data_simple <- data %>%
  group_by(subsite) %>%
  summarize(mean = mean(mean),
            lat = mean(lat),
            lon = mean(lon)) %>%
  as.data.frame

# plot
ggplot(wi_map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = 'black', fill = 'white') +
  coord_equal() +
  geom_point(data = data_simple, aes(x = lon, y = lat, color = mean))

# write out ---------------------------------------------------------------

write.csv(data, '/home/annie/Documents/MSU_postdoc/lter/data/modis_lst_NTL.csv')
