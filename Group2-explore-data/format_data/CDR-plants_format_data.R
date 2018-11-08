#####################################################
# Cedar Creeek Aboveground Biomass #
# Aldo 05/15/2018                                    #
#####################################################

rm(list = ls())
library(dplyr)
library(tidyverse)
library(testthat)

# your working directory
# setwd('C:/CODE/ltermetacommunities')
source("Group1-finding-data/util.R")


# Package ID: knb-lter-cdr.14.8 Cataloging System:https://pasta.edirepository.org.
# Data set title: Plant aboveground biomass data: Long-Term Nitrogen Deposition: Population, Community, and Ecosystem Consequences.
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
                              "Biomass"    ), check.names=TRUE) %>% 
              # remove duplicates!!!
              unique


#ALTERNATIVE: read from cached version on Google Drive          
# dt1 <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/CDR-plants/archive_knb-lter-cdr/e001_Plant aboveground biomass data.txt", sep="\t")  


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
                           VARIABLE_UNITS   = "BIOMASS",
                           VARIABLE_NAME    = as.character(VARIABLE_NAME) ) %>% 
                    order_col %>% 
                    # remove non-plants
                    subset( VARIABLE_NAME != 'Fungi') %>% 
                    subset( VARIABLE_NAME != 'Miscellaneous litter') %>% 
                    subset( VARIABLE_NAME != 'Mosses & lichens') %>%  
                    # fix clear mistakes
                    mutate( VARIABLE_NAME = replace(VARIABLE_NAME, 
                                                    VARIABLE_NAME == 'carex sp.', 
                                                    'Carex sp.') ) %>% 
                    mutate( VARIABLE_NAME = replace(VARIABLE_NAME, 
                                                      VARIABLE_NAME == 'cyperus sp.', 
                                                      'Cyperus sp.') )
                  

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



# clean species names ----------------------------------------------------------------

# check each 'sp.': KEEP if the only genus present
gen_sp   <- spp_abundance$VARIABLE_NAME %>% as.character %>% unique %>% sort %>%  
              grep('sp\\.',., value=T) %>% 
              gsub('sp\\.','',.) %>% 
              trimws

# proportion genus data
prop_gen <- function(x){
  
   spp_abundance %>% 
      subset( grepl(x, VARIABLE_NAME) ) %>% 
      group_by(VARIABLE_NAME) %>% 
      summarise( total = sum(VALUE) ) %>% 
      arrange( total ) %>% 
      mutate( prop = total/sum(total) ) %>% 
      arrange( desc(prop) ) %>% 
      as.data.frame
  
}

# list of proportions
prop_l <- lapply(gen_sp, prop_gen) %>% setNames( gen_sp )

# list of species to lump into a genera
lump_gen <- function(x){
  
  proportion_genus <- subset(x, grepl('sp\\.',VARIABLE_NAME) ) 
  if(proportion_genus$prop > 0.01) return( proportion_genus$VARIABLE_NAME )

}

# list of genus level to DROP
drop_gen <- function(x){
  
  proportion_genus <- subset(x, grepl('sp\\.',VARIABLE_NAME) ) 
  if(proportion_genus$prop < 0.01) return( proportion_genus$VARIABLE_NAME )

}

lump_vec <- sapply(prop_l, lump_gen) %>% unlist %>% gsub(' sp\\.', '', .)
drop_vec <- sapply(prop_l, drop_gen) %>% unlist

# update spp_abundance to "lump" and "drop" species abundance

# drop genus information
spp_abundance <- spp_abundance %>% 
                    subset( !(VARIABLE_NAME %in% drop_vec) )  
    
# lump species information
for(ii in 1:length(lump_vec)){
  
  spp_abundance <- spp_abundance %>% 
                     mutate( VARIABLE_NAME = replace(VARIABLE_NAME,
                                                     grepl(lump_vec[ii], VARIABLE_NAME),
                                                     paste0(lump_vec[ii], ' sp.') ) 
                            )
}


# CHECK there should be no "double" genuses
spp_abundance %>% 
  separate(VARIABLE_NAME, into = c('genus', 'species')) %>% 
  .$genus %>%
  unique %>% 
  table %>% 
  unique %>% 
  expect_equal(1)

# remove duplicates and introduce zeros
biomass_d <- spp_abundance %>% 
                # remove duplicates: sum over all species by site combination
                group_by(OBSERVATION_TYPE,SITE_ID,
                         DATE,VARIABLE_NAME,VARIABLE_UNITS) %>% 
                summarise( VALUE = sum(VALUE) ) %>% 
                ungroup %>% 
                # introduce zeros
                spread(key = VARIABLE_NAME, value = VALUE, fill = 0) %>% 
                gather(key = VARIABLE_NAME, value = VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS) 

# outfiles
form_cdr <- Reduce(function(...) rbind(...), 
                   list(biomass_d, spatialLocation, fire_d) ) %>% 
              # data until 2004 - because all sites represented
              # NOTE: still need to figure out if 4 sites should be considered
              # part of same community (share species pool? Dispersal among patches?)
              subset( DATE < 2005 )

# separate dataset in sites A,B,C and site D
cdr_abc  <- subset(form_cdr, !grepl('D_', SITE_ID) )
cdr_d    <- subset(form_cdr, grepl('D_', SITE_ID) )

# Write CSV file for cleaned data (L3)
write.csv(cdr_abc, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-cdr-plantsABC-compagnoni.csv", row.names = F)
write.csv(cdr_d,   file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-cdr-plantsD-compagnoni.csv", row.names = F)
