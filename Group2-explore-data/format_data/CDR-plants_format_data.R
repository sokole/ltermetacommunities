#####################################################
# Cedar Creeek Aboveground Biomass #
# Aldo 05/15/2018                                    #
#####################################################

rm(list = ls())
library(dplyr)
library(tidyverse)



# Package ID: knb-lter-cdr.386.8 Cataloging System:https://pasta.edirepository.org.
# Data set title: Plant aboveground biomass data: BAC: Biodiversity and Climate.
# Data set creator:  David Tilman -  
# Metadata Provider:    - Cedar Creek LTER 
# Contact:  Dan Bahauddin - Information Manager Cedar Creek Ecosystem Science Reserve  - webmaster@cedarcreek.umn.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/14/8/057e39850bd748d364df8a5ef60bb08d" 
infile1  <- sub("^https","http",infile1) 
dt1      <-read.csv(infile1,header=F ,skip=1
                            ,sep="\t"  
                  , col.names=c(
                              "Exp",     
                              "Year",     
                              "Field",     
                              "Plot",     
                              "NTrt",     
                              "NAdd",     
                              "NitrAdd",     
                              "NAtm.plus.NAdd",     
                              "Species",     
                              "Biomass"    ), check.names=TRUE)            
          

# format data ---------------------------------------------------

# select only control
form_d <- dt1 %>% 
            subset(NTrt == 1) %>% 
            mutate(site_id = paste(Field, Plot, sep='_') )


#alternately, use Google Drive File Stream:
#dt1 <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/CDR-plants/archive_knb-lter-cdr/e249_Plant aboveground biomass data.txt", stringsAsFactors=FALSE)  


# Species abundance data
spp_abundance <- form_d %>% 
                    rename(VALUE = Biomass,
                           SITE_ID = site_id,
                           VARIABLE_NAME = Species,
                           DATE = Year) %>% 
                    mutate(OBSERVATION_TYPE = "TAXON_ABUNDANCE",
                           VARIABLE_UNITS = "BIOMASS") %>% 
                    order_col

# spatial location
spatialLocation <- data.frame(
                        OBSERVATION_TYPE=rep("SPATIAL_COORDINATE",8),
                        SITE_ID=c(LETTERS[1:4],LETTERS[1:4]),
                        DATE=rep(NA,8),
                        VARIABLE_NAME=c(rep("latitude",4),rep("longitude",4)),
                        VARIABLE_UNITS=rep("dec. degrees",8),
                        VALUE=c(rep(45.4038890,4),rep(-93.2680560,4))
                    )

# fire information


# 
form_cdr <- rbind(spp_abundance, spatialLocation)

# Write CSV file for cleaned data (L3)
write.csv(form_cdr, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-cdr-plants-compagnoni.csv", row.names = F)
               