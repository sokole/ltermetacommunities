## Script to transform NTL Fish data into the long format
## Authors: Timothy Nguyen and Julien Brun, NCEAS
## Contact: SciComp@nceas.ucsb.edu

## Modified by Nina Lany, April 25, 2018
# Notes: I couldn't find any provenance for the dataset that was in the L3 folder, so I instead read in the most recent data from EDI. The EDI data package had been updated to include the most recent year (2017) and a problem with how some species codes were entered had been fixed (removed space at end of code). This fixed the suspicious jump in cumulative number of taxa around 2012). 


# Package ID: knb-lter-ntl.7.24 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Fish Abundance 1981 - current.
# Data set creator:  NTL Lead PI - University of Wisconsin 
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Metadata Provider:  NTL Information Manager - University of Wisconsin 
# Contact:  NTL Information Manager -  University of Wisconsin  - ntl.infomgr@gmail.com
# Contact:  NTL Lead PI -  University of Wisconsin  - ntl.leadpi@gmail.com
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

# infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/7/24/307b9c3bb1bb42825c3514086fe43acc"
# infile1 <- sub("^https","http",infile1)
# my_data <-read.csv(infile1,header=F
#          ,skip=1
#            ,sep=","
#                ,quot='"'
#        , col.names=c(
#                    "lakeid",
#                    "year4",
#                    "gearid",
#                    "effort",
#                    "spname",
#                    "total_caught"    ), check.names=TRUE, stringsAsFactors=FALSE)
               
#Google Drive File Stream method
my_data <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/NTL-fish-Stanley-Lottig/archive_knb-lter-ntl/ntl7_v6.csv", stringsAsFactors=FALSE)  

str(my_data)        
        
library(tidyr)
library(tidyverse)
library(dplyr)


## Functions ----

make_data_long <- function(data) {
  long <- data %>% 
    # Name the fields as required by the data format
    rename(EFFORT_COUNT = total_effort, TAXON_COUNT = total_caught, SITE_ID = lakeid, DATE = year4, VARIABLE_NAME = spname) %>%
    # Compute the catch per unit effort as # of fish caught / effort
    mutate(VALUE = TAXON_COUNT/EFFORT_COUNT) %>%
    # Removing the 
    select(-TAXON_COUNT,-EFFORT_COUNT) %>%
    #Adding the extra columns for observation type and unit
    mutate(OBSERVATION_TYPE = "TAXON_COUNT") %>%
    mutate(VARIABLE_UNITS = "CPUE")
  return(long)
}


## -- MAIN --

# Check the Number of years of measurement
nb_year <- length(unique(my_data$year4))

# Check is some specific gear is rarely used
my_data %>% 
  group_by(gearid) %>% 
  summarize(n = length(unique(year4))) 

# => Seems like the gill nets VGN and VGN127 are rarely used
# => Also seems like ESHOCK is rarely used 

# Check how they look like 
my_data %>% filter(gearid=="VGN" | gearid=="VGN127" | gearid == "ESHOCK")

## Remove these gill nets type from the data (still need to ask about ESHOCK)
# Preparing the data for the reformatting and aggregating
my_df <- my_data %>%
  # remove gears that were not used in a systematic way (from phone call information)
  filter(gearid!="VGN" & gearid!="VGN127") %>%
  # Sum accross the gears for effort and number of fish caught
  group_by(year4, lakeid, spname) %>%
  summarise(total_caught = sum(total_caught), total_effort = sum(effort)) 

# transform the data into the long format
long_data <- make_data_long(my_df)

# check for unidentified taxa
unique(long_data$VARIABLE_NAME)
length(which(long_data$VARIABLE_NAME=="UNIDSUNFISH")) #23
length(which(long_data$VARIABLE_NAME=="UNIDMINNOW")) #24
length(which(long_data$VARIABLE_NAME=="UNIDSHINER")) #2
length(which(long_data$VARIABLE_NAME=="LARVALFISH")) #14
length(which(long_data$VARIABLE_NAME=="UNIDCHUB")) #1
length(which(long_data$VARIABLE_NAME=="UNIDENTIFIED")) #21
length(which(long_data$VARIABLE_NAME=="UNIDDARTER")) #3

# remove unidentified taxa and specific lakes
long_data <- long_data %>%
                #remove 88 rows with unidentified taxa
                filter(VARIABLE_NAME != "UNIDSUNFISH",          
                       VARIABLE_NAME != "UNIDMINNOW",
                       VARIABLE_NAME != "UNIDSHINER",
                       VARIABLE_NAME != "LARVALFISH",
                       VARIABLE_NAME != "UNIDCHUB",
                       VARIABLE_NAME != "UNIDENTIFIED",
                       VARIABLE_NAME != "UNIDDARTER" ) %>% 
  
                # Dropping the souther lakes (those at 43 latitude) so the bounding box is not an entire region.
                filter( !SITE_ID %in% c('FI','ME','MO','WI') ) %>% 

                # #NKL 05/14/2018  remove the two bog lakes (TB and CB) and remove years prior to xxxx for balanced sampling.
                filter( SITE_ID != "TB",          
                        SITE_ID != "CB" )

# Code to bring in the coordinates is below. Not included here.
#are the data propogated (zeros for species not observed)?
tapply(long_data$VALUE, list(long_data$SITE_ID,long_data$DATE), length) #NO

# Convert from long to wide and back to long to be sure that we have fully propagated taxa
wide_data <- spread(long_data, key = VARIABLE_NAME, value = VALUE, fill = 0) 
long_dat <- gather(wide_data, key = VARIABLE_NAME, value = VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS)
tapply(long_dat$VALUE, list(long_dat$SITE_ID,long_dat$DATE), length) #YES!


# write the L3 output file
write.csv(long_dat, file="~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-ntl-fish-stanleyLottig.csv", row.names=FALSE)


