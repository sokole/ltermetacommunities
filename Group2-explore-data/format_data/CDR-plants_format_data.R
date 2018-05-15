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

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/386/8/34db2945b271c1a142d73fec2e298917" 
infile1 <- sub("^https","http",infile1) 
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep="\t"  
        , col.names=c(
                    "Year",     
                    "Sample.Date",     
                    "Plot",     
                    "Heat.Treatment",     
                    "Species",     
                    "Mass..paren.g.per.m2.paren."    ), check.names=TRUE, stringsAsFactors = F)
                    
                    
                    
#alternately, use Google Drive File Stream:
#dt1 <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/CDR-plants/archive_knb-lter-cdr/e249_Plant aboveground biomass data.txt", stringsAsFactors=FALSE)  




# Write CSV file for cleaned data (L3)
write.csv(xxxxx, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-cdr-plants-compagnoni.csv", row.names = F)
               