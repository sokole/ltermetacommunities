library(tidyverse)
library(googledrive)


# get file list from google drive target dir
file_list <- googledrive::drive_ls(paste0('~/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space'))

# filter list to only include data sets
L3_data_list <- file_list %>% 
  filter(grepl('(?i)l3\\-', name) & !grepl('(?i)\\-data\\-list', name) & !grepl('(?i)readme', name))  


# doesn't work for all the data files for some reason. CAP data seem to have some sort of weird header that screws up the import. 
# Should work for the following:
  # [1] "L3-cdr-plants-compagnoni.csv"                    "L3-sev-grasshopper-compagnoni.csv"              
  # [3] "L3-jrn-677plantdensity-popler.csv"               "L3-sev-53pinonjuniper-popler.csv"               
  # [5] "L3-sev-53pinonjuniper-popler.csv"                "L3-fce-fish-rehageWet.csv"                      
  # [7] "L3-fce-fish-rehageDry.csv"                       "L3-cap-herps-banville.csv"                      
  # [9] "L3-ntl-fish-stanleyLottig.csv"                   "L3-hbr-birds-sillett.csv"                       
  # [11] "L3-mcr-fish-castorani.csv"                       "L3-mcr-inverts-castorani.csv"                   
  # [13] "L3-mcr-algae-castorani.csv"                      "L3-mcr-coral-castorani.csv"                     
  # [15] "L3-ntl-zooplankton-stanleyLottig.csv"            "L3-jrn-lizards-hope.csv"                        
  # [17] "L3-ntl-macroinvertebrate-stanleyLottig.csv"      "L3-nwt-plants-hallett.csv"                      
  # [19] "L3-sbc-sessileInverts-castorani.csv"             "L3-sbc-mobileInverts-castorani.csv"             
  # [21] "L3-sbc-fish-castorani.csv"                       "L3-sbc-algae-castorani.csv"                     
  # [23] "L3-sbc-experiment-sessile-inverts-castorani.csv" "L3-sbc-experiment-mobile-inverts-castorani.csv" 
  # [25] "L3-sbc-experiment-fish-castorani.csv"            "L3-sbc-experiment-algae-castorani.csv"          
  # [27] "L3-usvi-coral-castorani.csv"

# L3 rows that don't throw errors:
# L3_data_list$name[c(4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31)]

# example
i <- 4
data_in_i <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", L3_data_list$id[i]))
