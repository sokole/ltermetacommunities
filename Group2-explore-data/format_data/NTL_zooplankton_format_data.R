## Script to transform NTL Zooplankton data into the long format
## Authors: Timothy Nguyen and Julien Brun, NCEAS
## Contact: SciComp@nceas.ucsb.edu

## Modified by NKL 04/26/2018
## NOTES: No data provenance to original data file, so I read in the dataset directly from EDI. Also archived data package in Google Drive L0 folder. 


# Package ID: knb-lter-ntl.37.14 Cataloging System:https://pasta.lternet.edu.
# Data set title: North Temperate Lakes LTER: Zooplankton - Trout Lake Area 1982 - current.
# Data set creator:    - Center for Limnology 
# Data set creator:    - NTL LTER 
# Metadata Provider:    - North Temperate Lakes LTER 
# Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
# Contact:    - NTL LTER Information Manager University of Wisconsin  - infomgr@lter.limnology.wisc.edu
# Contact:    - NTL LTER Lead PI Center for Limnology  - leadpi@lter.limnology.wisc.edu
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-ntl.37.14
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

#infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/37/14/637918b87ed507ff2add85eb0f0dd8f6" 
#infile1 <- sub("^https","http",infile1) 
#zoop_data <-read.csv(infile1,header=F 
#          ,skip=1
#            ,sep=","  
#                ,quot='"' 
#        , col.names=c(
#                    "lakeid",     
#                    "year4",     
#                    "sample_date",     
#                    "station",     
#                    "species_code",     
#                    "species_name",     
#                    "density",     
#                    "individuals_measured",     
#                    "avg_length"    ), check.names=TRUE, #stringsAsFactors=FALSE)
               
  
#Google Drive File Stream method:
zoop_data <- read.csv("~/Google Drive FIle Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/NTL-Zooplankton-Stanley-Lottig/archive_knb-lter-ntl/zoopallnlsummarysnap.csv", stringsAsFactors=FALSE)  
  

library(tidyr)
library(dplyr)
library(tidyverse)

#check to see if sampling effort was the same across all years
en <- function(x) {length(unique(x))}
tapply(zoop_data$sample_date, list(zoop_data$lakeid, zoop_data$year4), en)
#Samples are taken fortnighly for a minimum of five sampling occasions per year. Range was 5 - 18 sampling locatins per year.
#SHOULD WE TAKE THE MEAN OR THE MAXIMUM? 

#Lake Tr was only sampled once in 2013. I'm guessing this is actually lake TR. CHange Tr to TR.
zoop_data$lakeid <- gsub("Tr", "TR", zoop_data$lakeid)

#check number of taxa per lake per year
tapply(zoop_data$density, list(zoop_data$lakeid, zoop_data$year4), length)


#aggregate to maximum density per year:
agg_density <- zoop_data %>%
  #tbl_df() %>%
  group_by(lakeid, year4, species_name) %>%
  summarise(agg_density=max(density)) %>%
  rename(DATE=year4, VARIABLE_NAME=species_name, VALUE=agg_density, SITE_ID=lakeid) %>%
  data.frame()

#add additional columns to dataframe
n <- dim(agg_density)[1]
OBSERVATION_TYPE <- rep("TAXON_COUNT", n)
VARIABLE_UNITS <- rep("max annual density; number per liter", n) # from metadata
my_zoop_long <- agg_density %>%
  cbind(OBSERVATION_TYPE, VARIABLE_UNITS) %>% # adding columns
  select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE)

head(my_zoop_long)

#take a peek at the taxa:
unique(my_zoop_long$VARIABLE_NAME)  #SOME ARE IDd TO SPECIES AND SOME TO GENUS!!
#some are missing a taxa and three entries indicate unknown taxa
length(which(my_zoop_long$VARIABLE_NAME == "")) #n = 19
length(which(my_zoop_long$VARIABLE_NAME == "UNKNOWN")) #31
length(which(my_zoop_long$VARIABLE_NAME == "UNIDENTIFIED")) #1
length(which(my_zoop_long$VARIABLE_NAME == "UNKNOWN EGG-SHAPED ROTIFER")) #4
length(which(my_zoop_long$VARIABLE_NAME == "UNKNOWN ROTIFER")) #110

#remove 165 rown with missing or unknown taxa
my_zoop_long <- my_zoop_long %>%
  dplyr::filter(VARIABLE_NAME != "",          
                VARIABLE_NAME != "UNKNOWN",
                VARIABLE_NAME != "UNIDENTIFIED",
                VARIABLE_NAME != "UNKNOWN EGG-SHAPED ROTIFER",
                VARIABLE_NAME != "UNKNOWN ROTIFER"
                )


#check that all lakes surveyed in all years:
tapply(my_zoop_long$SITE_ID, my_zoop_long$DATE, en) #only five lakes sampled in 1981
#drop 1981
my_zoop_long <- my_zoop_long %>%
  dplyr::filter(DATE > 1981)




#are the data propogated (zeros for species not observed)?
tapply(my_zoop_long$VALUE, list(my_zoop_long$SITE_ID,my_zoop_long$DATE), length) #NO

# Convert from long to wide and back to long to be sure that we have fully propagated taxa
wide_data <- spread(my_zoop_long, key = VARIABLE_NAME, value = VALUE, fill = 0) 
long_dat <- gather(wide_data, key = VARIABLE_NAME, value = VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS)
tapply(long_dat$VALUE, list(long_dat$SITE_ID,long_dat$DATE), length) #YES!




#Write out the L3 dataset
write.csv(long_dat, "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-ntl-zooplankton-stanleyLottig.csv", row.names=F)




#########Original code from Timpthy and Julien
## -- LOADING PACKAGES --
source("Group1-finding-data/NTL_coordinates.R") #tools containing function to read from Google drive
library(tidyr)
library(dplyr)


## -- CONSTANTS --
input_file_hash <- '0B7AABlvKD6WjM1Z2TlFlVjI4cmM'
output <- 'NTL_Macroinvertebrates_long.csv'


## -- MAIN --

# Read file from Google drive
zoop_data <- read_csv_gdrive(input_file_hash)

agg_density <- zoop_data %>%
  tbl_df() %>%
  group_by(lakeid, year4, species_name) %>%
  summarise(agg_density=mean(density)) %>%
  rename(DATE=year4, VARIABLE_NAME=species_name, VALUE=agg_density, SITE_ID=lakeid) %>%
  data.frame()

n <- dim(agg_density)[1]
OBSERVATION_TYPE <- rep("TAXON_COUNT", n)
VARIABLE_UNITS <- rep("density", n) # this is too vague but I can't specify with the data I was given

my_zoop_long <- agg_density %>%
  cbind(OBSERVATION_TYPE, VARIABLE_UNITS) %>% # adding columns
  select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE)

head(my_zoop_long)

# Bring the coordinates
coord_df <- ntl_coordinates_formater(coord_file_hash)

# combine the two dataframes
zoop_output <- rbind(coord_df,my_macroinv_long)

# write the ouput file
write.csv(zoop_output, file=output, row.names=FALSE)
