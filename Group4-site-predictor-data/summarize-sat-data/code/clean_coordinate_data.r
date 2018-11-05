#### Cleans up calculated polygon centroids
# centroids are calculated using this script: https://code.earthengine.google.com/435e76781a7fa9e23f908789445fbef9

# Written: ACS on 18 Sep 2018
# Last edited: ACS on 05 Nov 2018

# load packages -----------------------------------------------------------

library(dplyr)
library(XML)
library(rlist)

# read in data ------------------------------------------------------------

raw_data <- read.csv('/home/annie/Documents/MSU_postdoc/ltermetacommunities/data/lter_centroids.csv', header = TRUE)
ntl_data <- read.csv('/home/annie/Documents/MSU_postdoc/ltermetacommunities/data/LTER_coordinates.csv', header = TRUE)

# convert to proper classes
raw_data$description <- as.character(raw_data$description)
raw_data$.geo <- as.character(raw_data$.geo)
ntl_data$site <- as.character(ntl_data$site)
ntl_data$subsite <- as.character(ntl_data$subsite)

# clean data --------------------------------------------------------------

# site codes are stored in the very messy 'description' column
# coordinates are stored in the '.geo' column

# go through rows and convert html table to data table for each site
for (i in 1:nrow(raw_data)) {
  site_table <- readHTMLTable(raw_data$description[i])[[1]][-1,]
  # remove first bit of garble first
  coordinates <- gsub(pattern = '^[A-Za-z0-9,"{:]+\\[', replacement = "", x = raw_data$.geo[i])
  # then end, and split into lon/lat
  coordinates <- strsplit(gsub(pattern = '\\]\\}$', replacement = "", x = coordinates), ',')
  # add to a dataframe
  coordinates <- data.frame(lat = as.numeric(coordinates[[1]][2]), 
                            lon = as.numeric(coordinates[[1]][1]))
  
  # put everything together
  clean_data <- data.frame(site = site_table[2, 2], subsite = NA, 
                           lat = coordinates$lat, lon = coordinates$lon, 
                           area_ha = NA)
  
  if (i == 1) {
    data <- clean_data
  }
  else {
    data <- rbind(data, clean_data)
  }
}

# join centroids with NTL sites -------------------------------------------

data <- rbind(data, ntl_data)

# export ------------------------------------------------------------------

write.csv(data, '/home/annie/Documents/MSU_postdoc/ltermetacommunities/data/LTER_coordinates_all.csv')
