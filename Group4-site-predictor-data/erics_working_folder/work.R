library(tidyverse)

MSH_site_info <- read_csv('MSH_PLOT_DESCRIPTORS_only_sites_we_used.csv')


MSH_site_info %>% select(LONG, LAT) %>% geosphere::centroid()
#############################

NTL_site_info <- read_csv('NTL_lakes_lat_longs.csv')


NTL_site_info %>% 
  filter(lat > 44) %>%
  # filter(!grepl('(?i)_bog', subsite)) %>%
  select(lon, lat) %>% geosphere::centroid()

#############################

USVI_site_info <- read_csv('USVI_lat_longs.csv')


USVI_site_info %>% 
  select(lon, lat) %>% geosphere::centroid()

#############################

SBC_site_info <- read_csv('SBC_site_coords.csv')


SBC_site_info %>% 
  filter(lat > 34.1) %>%
  select(lon, lat) %>% 
  summarise_all(mean) %>% as.data.frame()

#############################
dat <- read_delim('KCDNRP-SamplesWithBioticAttributes.txt', delim = '\t')

dat_sites <- dat %>% filter(!duplicated(`Site Code`))

dat_sites %>% select(Longitude, Latitude) %>% geosphere::centroid()

#############################
dat <- read_csv('FountainCreek_20180330.1410.SiteInfo.csv')

dat_sites <- dat %>% filter(!duplicated(`SiteNumber`))

dat_sites %>% select(Longitude_dd, Latitude_dd) %>% geosphere::centroid()


