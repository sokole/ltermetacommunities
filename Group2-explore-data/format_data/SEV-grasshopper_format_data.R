# Aldo Compagnoni 5.15.2018
# formatting the SEVILLETTA grasshopper dataset


# Package ID: knb-lter-sev.106.214968 Cataloging System:https://pasta.lternet.edu.
# Data set title: Long-Term Core Site Grasshopper Dynamics for the Sevilleta National Wildlife Refuge, New Mexico (1992-2013).
# Data set creator:  David Lightfoot - SEV LTER 
# Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
# Contact:  Information Manager Sevilleta LTER -  SEV LTER  - data-use@sevilleta.unm.edu
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-sev.106.214968
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 
library(tidyr)
library(dplyr)

infile1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sev/106/214968/1def46af7a0b7d367705caaf2697f0d2" 
infile1 <- sub("^https","http",infile1)

dt1     <-read.csv(infile1,header=F ,skip=1,sep="," ,quot='"' 
                    , col.names=c(
                    "Date",     
                    "PER",     
                    "Site",     
                    "Web",     
                    "Transect",     
                    "Species",     
                    "AGE",     
                    "gender",     
                    "substrate",     
                    "Count",     
                    "BURNED",     
                    "Comments"), check.names=TRUE, stringsAsFactors=FALSE) 
#Alternately, read in using Google Drive File Stream
dt1 <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/SEV-106-grasshoppers/archive_knb-lter-sev/sev106_hopperdynamics_20150826.txt", col.names=c(
                    "Date",     
                    "PER",     
                    "Site",     
                    "Web",     
                    "Transect",     
                    "Species",     
                    "AGE",     
                    "gender",     
                    "substrate",     
                    "Count",     
                    "BURNED",     
                    "Comments"),stringsAsFactors=FALSE) 


# format 
count_d <- dt1 %>% 
            mutate( Species = replace(Species, 
                                      Species %in% c("M.ARIDUS"), 
                                      'MEAR') ) %>% 
            separate(col = 'Date', into = c('month','day','year') ) %>% 
            # sum counts over years, sex, age ground, and substrate
            group_by(year,Site,Web,Species) %>% 
            summarise( year_count = sum(Count) ) %>% 
            # create identifier for site/Web/Transect combination
            mutate( spatial_rep = paste(Site,Web, sep = '_') ) %>% 
            ungroup %>% 
            # select and format only relevant data
            select(year,spatial_rep, Species, year_count) %>% 
            rename(VALUE   = year_count,
                   SITE_ID = spatial_rep,
                   VARIABLE_NAME = Species,
                   DATE = year) %>% 
            mutate(OBSERVATION_TYPE = "TAXON_COUNT",
                   VARIABLE_UNITS = "Count (two censuses a year)" ) %>% 
            # introduce the zeros
            spread(key = VARIABLE_NAME, value = VALUE, fill = 0) %>% 
            gather(key = VARIABLE_NAME, value = VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS) %>% 
            # select LATR and BOGR (because longest rep with lots of shared species)
            subset( grepl('LATR|BOER',SITE_ID) ) %>% 
            subset( !(VARIABLE_NAME %in% 'NONE') )


# species codes (Aldo put this file in Google Drive L0). Not used here.
spp_d <- read.csv('~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/SEV-106-grasshoppers/sev106_spp_codes.csv',
                  sep=',', header=T)


# site_d <- data.frame(OBSERVATION_TYPE=rep("SPATIAL_COORDINATE",8),
#                      SITE_ID=c(),
#                         DATE=rep(NA,4),
#                         VARIABLE_NAME=c(rep("latitude",4),rep("longitude",4)),
#                         VARIABLE_UNITS=rep("dec. degrees",8),
#                         VALUE=c(rep(45.4038890,4),rep(-93.2680560,4))
#                     )

# lat/lon  Five Points Black Grama (BOER) -----------------------------------

# function that creates a data frame of lat/lon for the sites
create_coord <- function(site_name, coord_val, coord_val_name){

  expand.grid(OBSERVATION_TYPE = "SPATIAL_COORDINATE",
              SITE_ID          = count_d$SITE_ID %>% 
                                    unique %>% 
                                    grep(site_name,.,value=T),
              DATE             = NA,
              VARIABLE_NAME    = coord_val_name,
              VARIABLE_UNITS   = "dec. degrees",
              VALUE            = coord_val,
              stringsAsFactors = F)

}

# sources 
site_d <- Reduce(function(...) rbind(...), 
                 list(create_coord('BOER', 34.3331,  'latitude'), 
                      create_coord('BOER', -106.736, 'longitude'), 
                      # Creosotebush site (LATR)
                      create_coord('LATR', 34.3331,  'latitude'),
                      create_coord('LATR', -106.7358,  'longitude')
                      # Pinyon-Juniper (PJ)
                      #create_coord('PJ', 34.368,    'latitude'), 
                      #create_coord('PJ', -106.535 , 'longitude'), 
                      # The Blue Grama (BOGR)
                      #create_coord('BOGR', 34.3348,   'latitude'), 
                      #create_coord('BOGR', -106.631 , 'longitude') 
                      ) )

# out file
out   <- rbind(count_d, site_d)
write.csv(out, '~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-sev-grasshopper-compagnoni.csv', row.names=F)
