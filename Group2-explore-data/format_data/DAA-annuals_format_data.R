#Aldo Compagnoni June 2018
#NKL added data provenance
rm(list = ls(all = T))
options(stringsAsFactors = F)
library(dplyr)
library(tidyr)
library(stringi)


# DAA: Desert Annual Archive

# Package ID: No ID (for now)
# Data set title: 
# Data set creator:  
# Metadata Provider: 

# read from local drive: Ecological Archives site seems down
in_file <- 'C:/Users/ac22qawo/Dropbox/sApropos/data/veneable/30_year/'

daa_spp <- read.csv( paste0(in_file, 'Species_list_2016_2.csv') ) %>% 
              rename( VARIABLE_NAME = Species.code )
daa     <- read.csv( paste0(in_file, 'CensusData.csv') ) %>% 
            # count individuals
            count(Year, 
                  plot.habitat.replicate, plot_group, 
                  species ) %>% 
            mutate( SITE_ID = paste0(plot.habitat.replicate, 
                                     plot_group, 
                                     sep='_') ) %>% 
            # LTERMETACOMM FORMAT
            rename( VARIABLE_NAME = species,
                    DATE          = Year,
                    VALUE         = n ) %>% 
            mutate( OBSERVATION_TYPE = 'TAXON_COUNT',
                    VARIABLE_UNITS   = 'COUNT' ) %>% 
            # order data
            select(OBSERVATION_TYPE, SITE_ID, DATE, 
                   VARIABLE_NAME, VARIABLE_UNITS, VALUE)

# keep sites 
daa %>% 
  select(DATE, SITE_ID) %>% 
  unique %>% 
  count(DATE) %>% 


daa     <- read.csv( paste0(in_file, 'CensusData.csv') ) %>% 
            # count individuals
            count(Year, 
                  plot.habitat.replicate,
                  species ) %>% 
            rename( SITE_ID = plot.habitat.replicate ) %>% 
            # LTERMETACOMM FORMAT
            rename( VARIABLE_NAME = species,
                    DATE          = Year,
                    VALUE         = n ) %>% 
            mutate( OBSERVATION_TYPE = 'TAXON_COUNT',
                    VARIABLE_UNITS   = 'COUNT' ) %>% 
            # order data
            select(OBSERVATION_TYPE, SITE_ID, DATE, 
                   VARIABLE_NAME, VARIABLE_UNITS, VALUE)

      

daa
     
# keep sites present in every year
site_keep <- snail_long %>% 
                dplyr::select(DATE, SITE_ID) %>% 
                unique %>% 
                count(SITE_ID) %>% 
                subset( n == 27 ) %>% 
                .$SITE_ID

# select the common sites
snail_out <- snail_long %>% subset( SITE_ID %in% site_keep )

# write file out
write.csv(snail_out, '~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-luq-snails-compagnoni.csv', row.names=F)
