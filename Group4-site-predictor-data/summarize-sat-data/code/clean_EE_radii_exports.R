### Clean and combine output of earth engine LTER radii extraction script

## Written by ACS 14 Nov 2018
## Last edited by ACS 14 Nov 2018


# load packages -----------------------------------------------------------

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(maps)

# functions ---------------------------------------------------------------

combine_csvs <- function(file_list) {
  data <- map_dfr(file_list, read.csv, header = TRUE)
  return(data)
}

clean_data <- function(data, data_name) {
  # check if the variable contains multiple years
  ts <- length(grep('year', names(data))) > 0
  
  # convert year to column
  if (ts == TRUE) {
    data <- data %>%
      gather(key = 'year', value = 'value', names(data)[grep('year', names(data))]) %>%
      tidyr::extract(year, c("year", "variable"), "year(\\d+)\\_([a-zA-z]+)") %>%
      spread(variable, value) %>%
      mutate(year = as.numeric(year))
  } else {
    data <- as_tibble(data) 
  }
  
  # rename mean, std to indicate variable
  names(data)[grep('mean', names(data))] <- paste('mean', data_name, sep = '_')
  names(data)[grep('stdDev', names(data))] <- paste('sd', data_name, sep = '_')
  
  # remove unnecessary columns, add latitude
  data <- data %>%
    dplyr::select(-.geo, -system.index, -notes) %>%
    left_join(site_data, by = c('site', 'subsite', 'lon')) %>%
    dplyr::select(-notes, -area_ha.y) %>%
    rename(area_ha = area_ha.x) 
  
  # order columns nicely
  if (ts == TRUE) {
    data <- data %>%
      dplyr::select(4, 5, 9, 2, 1, 6, 3, 7, 8) %>%
      arrange(site, subsite, radius, year)
  } else {
    data <- data %>%
      dplyr::select(5, 7, 8, 2, 1, 4, 3, 6) %>%
      arrange(site, subsite, radius)
  }
  
  return(data)
}

# import data -------------------------------------------------------------

fp <- '/home/annie/Documents/MSU_postdoc/lter/data/radii_data/'
all_files <- list.files(path = fp, pattern = 'lter_radii', full.names = TRUE, recursive = TRUE)

# elevation (meters)
elev_files <- all_files[grep('elev', all_files)]
elev_data <- combine_csvs(file_list = elev_files)

# land surface temperature (kelvin)
lst_files <- all_files[grep('lst', all_files)]
lst_data <- combine_csvs(file_list = lst_files)

# normalized difference vegetation index (unitless)
ndvi_files <- all_files[grep('ndvi', all_files)]
ndvi_data <- combine_csvs(file_list = ndvi_files)

# sea surface temperature (celcius)
sst_files <- all_files[grep('sst', all_files)]
sst_data <- combine_csvs(file_list = sst_files)

# sea surface temperature (celcius)
chla_files <- all_files[grep('chla', all_files)]
chla_data <- combine_csvs(file_list = chla_files)

# sea surface temperature (celcius)
bath_files <- all_files[grep('bath', all_files)]
bath_data <- combine_csvs(file_list = bath_files)

# original site data (for coordinates)
site_data <- read.csv('/home/annie/Documents/MSU_postdoc/ltermetacommunities/Group4-site-predictor-data/summarize-sat-data/data/LTER_coordinates_all.csv', stringsAsFactors = FALSE)

# clean data --------------------------------------------------------------

elev_data <- clean_data(elev_data, 'elev')
lst_data <- clean_data(lst_data, 'lst')
ndvi_data <- clean_data(ndvi_data, 'ndvi')
sst_data <- clean_data(sst_data, 'sst')
chla_data <- clean_data(chla_data, 'chla')
bath_data <- clean_data(bath_data, 'bath')

full_data <- elev_data %>%
  left_join(lst_data) %>%
  full_join(ndvi_data) %>%
  full_join(sst_data) %>%
  full_join(chla_data) %>%
  full_join(bath_data)

# fix ndvi to be 0-100 just like chl-a
full_data$mean_ndvi <- full_data$mean_ndvi * 100
full_data$sd_ndvi <- full_data$sd_ndvi * 100

# export ------------------------------------------------------------------

write.csv(full_data, '/home/annie/Documents/MSU_postdoc/lter/data/lter_radii_data.csv', row.names = FALSE)

# plot --------------------------------------------------------------------

mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders

ggplot(full_data, aes(x = lon, y = lat, col = mean_lst)) +
  mapWorld +
  geom_point(size = 4)
