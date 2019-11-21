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
                                     '_',
                                     plot_group) ) %>% 
            # LTERMETACOMM FORMAT
            rename( VARIABLE_NAME = species,
                    DATE          = Year,
                    VALUE         = n ) %>% 
            mutate( OBSERVATION_TYPE = 'TAXON_COUNT',
                    VARIABLE_UNITS   = 'COUNT' ) %>% 
            # order data
            select(OBSERVATION_TYPE, SITE_ID, DATE, 
                   VARIABLE_NAME, VARIABLE_UNITS, VALUE) %>% 
            # introduce zeros
            spread(key = VARIABLE_NAME, value = VALUE, fill = 0) %>% 
            gather(key = VARIABLE_NAME, value = VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS) 

# keep sites 
keep_sites <- daa %>% 
                select(DATE, SITE_ID) %>% 
                unique %>% 
                count(SITE_ID) %>%
                # only keep the sites with highest temporal replication
                subset(n > 28) %>% 
                .$SITE_ID

daa_out    <- daa %>% subset( SITE_ID %in% keep_sites )

# write file out
write.csv(daa_out, 
          'C:/L3-daa-plants-compagnoni.csv',
          row.names=F)
