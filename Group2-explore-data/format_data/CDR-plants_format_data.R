#####################################################
# Cedar Creeek Aboveground Biomass #
# Aldo 05/15/2018                                    #
#####################################################

rm(list = ls())
library(dplyr)
library(tidyverse)

# your working directory
source("Group1-finding-data/util.R")


# Package ID: knb-lter-cdr.14.8 Cataloging System:https://pasta.edirepository.org.
# Data set title: Plant aboveground biomass data: Long-Term Nitrogen Deposition: Population, Community, and Ecosystem Consequences.
# Data set creator:  David Tilman -  
# Metadata Provider:    - Cedar Creek LTER 
# Contact:  Dan Bahauddin - Information Manager Cedar Creek Ecosystem Science Reserve  - webmaster@cedarcreek.umn.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/386/8/34db2945b271c1a142d73fec2e298917

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


#ALTERNATIVE: read from cached version on Google Drive          
dt1 <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/CDR-plants/archive_knb-lter-cdr/e001_Plant aboveground biomass data.txt", sep="\t")  


# format data ---------------------------------------------------

# select only control
form_d <- dt1 %>% 
            subset(NTrt == 1) %>% 
            mutate(site_id = paste(Field, Plot, sep='_') )
  


# Species abundance data
spp_abundance <- form_d %>% 
                    rename(VALUE = Biomass,
                           SITE_ID = site_id,
                           VARIABLE_NAME = Species,
                           DATE = Year) %>% 
                    mutate(OBSERVATION_TYPE = "TAXON_COUNT",
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

# fire information (years reported in online metadata)
fire_d<- list( expand.grid(SITE_ID = spp_abundance$SITE_ID %>% 
                                        grep('D',.,value=T) %>% unique, 
                           DATE    = c(1981, 1982, 1988, 1990, 1991, 
                                       1992, 1993, 1994, 1996, 1997, 
                                       1999, 2000, 2002, 2003, 2005, 
                                       2006, 2008, 2009, 2011, 2012, 2014),
                           stringsAsFactors = F),
                expand.grid(SITE_ID = spp_abundance$SITE_ID %>% 
                                        grep('A|B|C',.,value=T) %>% unique, 
                            DATE    = c( 2005, 2006, 2007, 2008, 2009, 
                                         2010, 2011, 2012, 2013, 2014),
                            stringsAsFactors = F) ) %>% 
            # put this all together
            Reduce(function(...) rbind(...), .) %>% 
            # flag the years where fires occurred 
            mutate(fire = 'yes',
                   DATE = as.numeric(DATE)) %>% 
            full_join( unique(select(spp_abundance, SITE_ID, DATE)) ) %>% 
            mutate(fire = replace(fire, is.na(fire), 'no') ) %>% 
            # format based on ltermetacomm data standard!
            format_data(OBSERVATION_TYPE = "ENV_VAR", 
                        SITE_ID = "SITE_ID", 
                        DATE = 'DATE', # NA because non-temporal
                        VARIABLE_NAME = "prescribed_fire", 
                        VARIABLE_UNITS = NA,  
                        VALUE = "fire")

# outfile
form_cdr <- Reduce(function(...) rbind(...), list(spp_abundance, spatialLocation, fire_d) )

# Write CSV file for cleaned data (L3)
write.csv(form_cdr, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-cdr-plants-compagnoni.csv", row.names = F)
               