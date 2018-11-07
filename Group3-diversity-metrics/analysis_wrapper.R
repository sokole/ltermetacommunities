#######################################################
# Calculates compositional turnover at local (CV_alpha) and regional (CV_gamma) scales
# Calculates phi, which is scaling of variability, as CV_gamma / CV_alpha
# version 1.0.2 from ltermetacommunities group 3 directory on github
# Author: Eric Sokol (sokole@gmail.com), modified by Nathan Wisnoski 
# based on work in collaboration with Thomas Lamy

options(stringsAsFactors = FALSE)

#########################
# libraries
#########################
library(tidyverse)
library(googledrive)
library(ggrepel)

#########################
# source all the functions for metacommunity analysis
#########################
source("metacommfun/R/bd_vegan.R")
source("metacommfun/R/comp_stability_components.R")


#######################################################
# -- download list of data sets off google drive using google-id
#######################################################

working_dir <- drive_ls(path = as_id("0BxUZSA1Gn1HZamlITk9DZzc1c1E"))
data_list <- working_dir %>% filter(grepl('(?i)\\.csv', name))

# # google id for L3-DATA-list google sheet
# id_google_data_list <- '17IKwyA1zniMP15kM8hMe_zCuYfML74_7l2io7FhaiBo'
# 
# 
# # download L3-DATA-list as a .csv
# download.link <- paste0("https://docs.google.com/spreadsheets/export?id=",
#                         id_google_data_list,
#                         "&format=csv")
# data_list <- read.csv(file = download.link, 
#                       header = T,
#                       stringsAsFactors = FALSE) 
# 
# #find google.id column, and rename 'google.id' 
# col_names <- names(data_list)
# col_names[grep('google.id',col_names)] <- 'google.id'
# names(data_list) <- col_names
# 
# #only keep necessary columns
# data_list <- data_list %>% 
#   filter(LTER.site != 'EXAMPLE') %>%
#   filter(nchar(google.id) > 0)
# 

#######################################################
# -- Loop through data sets and call functions
#######################################################


# loop to read in data, call wrapper function, write results to data_ALL
data_ALL <- data.frame()
for(i in 1:nrow(data_list)){
  try_result <- try({
    
    # get record
    i_data_record <- data_list[i,]
    
    # get google id used to download data
    data_id_googledrive <- i_data_record$id
    
    # link to read in spreadsheet from google drive
    download.link <- paste0("https://drive.google.com/uc?export=download&id=",
                            data_id_googledrive)
    
    # blank data frame
    d.in.long <- data.frame()
    
    # read data into data.frame and filter out non-taxon data, spp names that are NAs, and negative VALUES
    # make SITE_ID and DATE chars
    d.in.long <- read.csv(file = download.link, header = T,
                          stringsAsFactors = FALSE) %>%
      filter(OBSERVATION_TYPE == 'TAXON_COUNT') %>% 
      filter(!is.na(VARIABLE_NAME)) %>%
      filter(VALUE >= 0) %>%
      mutate(SITE_ID = as.character(SITE_ID),
             DATE = as.character(DATE))
    
    if('TREATMENT' %in% names(d.in.long)){
      d.in.long <- d.in.long %>%
        filter(TREATMENT %in% c(NA, 'NA', '', 'control', 'Control','CONTROL'))
    }
    
    key_list <- with(d.in.long, 
                     paste(
                       OBSERVATION_TYPE,
                       SITE_ID,
                       DATE,
                       VARIABLE_NAME,
                       VALUE,
                       sep = '_'
                     ))
    
    # get rid of exact dupes
    rows2check <- which(duplicated(key_list))
    if(length(rows2check) > 0){
      d.in.long <- data.frame(d.in.long[-rows2check,], row.names = NULL)
    }
    
    # take mean of replicate observations that are not exact dupes
    d.in.long <- d.in.long %>% group_by(
      OBSERVATION_TYPE, SITE_ID, DATE, 
      VARIABLE_NAME, VARIABLE_UNITS) %>%
      summarise(VALUE = mean(VALUE)) %>% data.frame()
    
    # Send data to function to calclulate compositional variability and scaling of variability
    d.bd <- data.frame()
    if(nrow(d.in.long) > 0){
      d.bd <- data.frame(
        i_data_record[,c("name","id")],
        comp_stability_components(d.in.long,
                                     location_name = 'SITE_ID',
                                     time_step_name = 'DATE',
                                     taxon_name = 'VARIABLE_NAME',
                                     taxon_count_name = 'VALUE'))
      d.bd$start_year <- min(as.numeric(d.in.long$DATE))
      d.bd$end_year <- max(as.numeric(d.in.long$DATE))
      
      d.bd <- d.bd %>% mutate(
        n_years = end_year - start_year,
        alpha_temporal_bd_rate = mean_alpha_temporal_bd / n_years,
        gamma_temporal_bd_rate = gamma_temporal_bd / n_years
      )
      
      data_ALL <- rbind(
        data_ALL,
        d.bd)
    }
    
  })
  print(i_data_record)
}

#write results locally
write.csv(data_ALL, 
          file.path("Group3-diversity-metrics", 
                    paste0('dat-comp-variability-components_',Sys.Date(),'.csv')),
          row.names = FALSE)

#######################################################
# -- Make figures
#######################################################


# link to read in spreadsheet from google drive
l0_id <- "1wP_-hmmB81cpGklhBZRZybDrVOWaDX2rijBVcrlAdfQ"

download_l0 <-paste0("https://docs.google.com/spreadsheets/export?id=",
                                             l0_id,
                                             "&format=csv")

l0 <- read_csv(file = download_l0)

data_cats <- l0 %>% select(`data directory`, organism, `body size`, `dispersal type`, `trophic group`, biome) %>% 
  mutate(name = paste0("L3-", str_to_lower(`data directory`), ".csv")) %>% 
  select(-'data directory')


data_ALL %>% 
  gather(gamma_temporal_bd, mean_alpha_temporal_bd, phi_bd, key = "scale", value = "bd") %>% 
  ggplot(aes(x = n_locations, y = bd)) + 
  facet_grid(scale ~ .) +
  geom_point() + 
  geom_smooth()
