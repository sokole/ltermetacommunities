## Script to transform NTL Fish data into the long format
## Authors: Timothy Nguyen and Julien Brun, NCEAS
## Contact: SciComp@nceas.ucsb.edu

## Modified by Nina Lany, April 25, 2018
# Notes: I couldn't find any provenance for the dataset that was in the L3 folder, so I instead read in the most recent data from EDI. The EDI data package had been updated to include the most recent year (2017) and a problem with how some species were identified had been fixed (so there is no longer a suspicious jump in cumulative number of taxa around 2010). 


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

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/7/24/307b9c3bb1bb42825c3514086fe43acc" 
infile1 <- sub("^https","http",infile1) 
my_data <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "lakeid",     
                    "year4",     
                    "gearid",     
                    "effort",     
                    "spname",     
                    "total_caught"    ), check.names=TRUE, stringsAsFactors=FALSE)
               
#alternative if no internet connection: read from local Google Drive
#my_data <- read.csv("~/Google Drive/LTER Metacommunities/LTER-DATA/L0-raw/NTL-fish-Stanley-Lottig/archive_knb-lter-ntl/ntl7_v6.csv", stringsAsFactors=FALSE)  

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

# Code to bring in the coordinates is below. Not included here.
#are the data propogated (zeros for species not observed)?
tapply(long_data$VALUE, list(long_data$SITE_ID,long_data$DATE), length) #NO

# Convert from long to wide and back to long to be sure that we have fully propagated taxa
wide_data <- spread(long_data, key = VARIABLE_NAME, value = VALUE, fill = 0) 
long_dat <- gather(wide_data, key = VARIABLE_NAME, value = VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS)
tapply(long_dat$VALUE, list(long_dat$SITE_ID,long_dat$DATE), length) #YES!



# write the L3 output file
write.csv(long_dat, file="~/Google Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-ntl-fish-stanleyLottig.csv", row.names=FALSE)





###################################################
###################################################
#Old - trying to tind the file with the google ID hashgiven below. It's in ARCHIVE. Did not use.        
my_data <-  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", input_file_hash), stringsAsFactors=F) 


#ORIGINAL SCRIPT FROM TIMOTHY AND JULIEN
## -- LOADING PACKAGES --

source("Group1-finding-data/NTL_coordinates.R") #tools containing function to read from Google drive
library(tidyr)
library(dplyr)


## -- CONSTANTS --

input_file_hash <- "0B7AABlvKD6WjSTY3YUVKZ1AwLWs"
output_file <- "NTL_Fish_long.csv"


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

# Read file from Google drive
my_data <- read_csv_gdrive(file_hash)

# Check the Number of years of measurement
nb_year <- length(unique(my_data$year4))

# Check is some specific gear is rarely used
my_data %>% 
  group_by(gearid) %>% 
  summarize(n = length(unique(year4))) 

# => Seems like the gill nets VGN and VGN127 are rarely used

## Remove these gill nets type from the data

# Check how they look like 
my_data %>% filter(gearid=="VGN" | gearid=="VGN127")

# Preparing the data for the reformatting and aggregating
my_df <- my_data %>%
  # remove gears that were not used in a systematic way (from phone call information)
  filter(gearid!="VGN" & gearid!="VGN127") %>%
  # Sum accross the gears for effort and number of fish caught
  group_by(year4, lakeid, spname) %>%
  summarise(total_caught = sum(total_caught), total_effort = sum(effort)) 

# transform the data into the long format
long_data <- make_data_long(my_df)

# Bring the coordinates
coord_df <- ntl_coordinates_formater(coord_file_hash)

# combine the two dataframes
long_data_coord <- ungroup(long_data) %>% rbind(coord_df,.)

# write the ouput file
write.csv(long_data_coord, file=output_file, row.names=FALSE)
