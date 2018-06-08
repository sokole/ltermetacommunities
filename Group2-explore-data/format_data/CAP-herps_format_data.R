#################################################################################################
# Long-term monitoring of herpetofauna along the Salt and Gila Rivers in and near the greater Phoenix metropolitan area, ongoing since 2012
# RA 05/15/2018                                    
#################################################################################################

rm(list = ls())
# Check for and install required packages
for (package in c('dplyr', 'tidyverse', 'tidyr')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, dependencies=TRUE, repos='http://cran.rstudio.com/')
    library(package, character.only=T)
  }
}

# Package ID: knb-lter-cap.627.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: Long-term monitoring of herpetofauna along the Salt and Gila Rivers in and near the greater Phoenix metropolitan area, ongoing since 2012.
# Data set creator:  Heather Bateman - Arizona State University 
# Data set creator:  Dan Childers - Arizona State University 
# Metadata Provider:  Mélanie Banville - Arizona State University 
# Metadata Provider:  Stevan Earl - Arizona State University 
# Metadata Provider:  Sally Wittlinger - Arizona State University 
# Contact:    - Data Manager Julie Ann Wrigley Global Institute of Sustainability, Arizona State University  - caplter.data@asu.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

#this is the site table (site chatacteristics)
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cap/627/3/aec6f02c5fe993ebfcdbba0644225f9d" 
infile1 <- sub("^https","http",infile1) 
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "reach",     
                    "urbanized",     
                    "restored",     
                    "water",     
                    "observation_date",     
                    "observer_intitials",     
                    "common_name",     
                    "scientific_name",     
                    "quantity",     
                    "sampling_events_notes",     
                    "sampling_events_observation_notes"    ), check.names=TRUE)
               
        
#this is the survey table (counts)
infile2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cap/627/3/ad24abcb8781e1ff6c7ef1b2a4ec1eb9" 
infile2 <- sub("^https","http",infile2) 
 dt2 <-read.csv(infile2,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "reach",     
                    "urbanized",     
                    "restored",     
                    "water",     
                    "observation_date",     
                    "transect",     
                    "location",     
                    "time_start",     
                    "time_end",     
                    "common_name",     
                    "scientific_name",     
                    "quantity",     
                    "surveys_notes",     
                    "surveys_observation_notes"    ), check.names=TRUE, stringsAsFactors=FALSE)
               
            
        

#alternately, use Google Drive File Stream:
#this is the correct file to read in
dt2 <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/CAP-herps-Banville/archive_knb-lter-cap.627.3_152642411917174235/627_herp_survey_observations_3bfc57f795b316b581f110042dff4230.csv", stringsAsFactors=FALSE) 

#subset species data (NOTE THAT URBANIZED, RESTORED, AND WATER CATEGORIES COULD POTENTIALLY BE ENV COVARIATES)
dt2 <- dt2 %>% select(reach,urbanized,restored,water,observation_date,common_name,scientific_name,quantity)
head(dt2)

#Changing column format 
dt2$reach <- as.character(dt2$reach) #change site code to character to subset
dt2$scientific_name <- as.character(dt2$scientific_name) #change species code to character to subset

#Extract year and name seasonal observations
dt2$Date <- as.POSIXct(dt2$observation_date)
dt2$Year <- as.numeric(format(dt2$Date, format = "%Y"))
dt2$Month <- as.numeric(format(dt2$Date, format = "%m"))
unique(dt2$Year) #number of years with observations 
unique(dt2$Month) #number of months with observations 

#Rename months to sampling season blocks
dt2 <- dt2 %>% mutate(Month = replace(Month, Month == 4, 'AprilMay'), 
               Month = replace(Month, Month == 5, 'AprilMay'),
               Month = replace(Month, Month == 6, 'JuneJuly'),
               Month = replace(Month, Month == 7, 'JuneJuly'),
               Month = replace(Month, Month == 9, 'SeptOct'),
               Month = replace(Month, Month == 10, 'SeptOct'))

#Check sampling within each year at each reach
en <- function(x) {length(unique(x))} 
tapply(dt2$Year, list(dt2$reach, dt2$Month), en) #SUBSET BY ESCA for 2000-onward dataset
tapply(dt2$Month, list(dt2$reach, dt2$Year), en) 

#Drop march because it was only sampled 1 year, drop 2012 because only sampled twice
#Scientific name is already NA if unidentified
dat <- dt2 %>% filter(Month != 3, Year != 2012)

#Recheck sampling within each year at each reach
tapply(dat$Year, list(dat$reach, dat$Month), en) #SUBSET BY ESCA for 2000-onward dataset
tapply(dat$Month, list(dat$reach, dat$Year), en) 

#       2013 2014 2015 2016 2017
#Ave35     3    3    3    3    3
#Ave67     3    3    3    3    3
#BM        3    3    3    3    3
#Price     3    3    3    3    2
#Priest    3    3    3    3    2
#Rio       3    3    3    3    3
#Tonto     3    3    3    3    3

#Remove unidentified species and NAs
data <- dat %>% filter(!is.na(scientific_name))

#calculate the maximum number of each species observed at each point in each year (see Banville and Bateman 2011 for supporting methods)
out <- data.frame(OBSERVATION_TYPE = "", 
                  SITE_ID = "",
                  DATE = "",
                  VARIABLE_NAME = "",
                  VARIABLE_UNITS = "",
                  VALUE = "")

step1 <- data %>% group_by(Year, reach, scientific_name) %>% 
  summarize(NUM = sum(quantity))

step2 <- step1 %>% group_by(Year, reach, scientific_name) %>% 
  summarize(VALUE = max(NUM))

out <- cbind.data.frame(OBSERVATION_TYPE = "TAXON_COUNT",
                        SITE_ID = step2$reach,
                        DATE = step2$Year,
                        VARIABLE_NAME = step2$scientific_name,
                        VARIABLE_UNITS = "count",
                        VALUE = step2$VALUE)

# Convert from long to wide and back to long to be sure that we have fully propagated taxa
out_wide <- spread(out, key = VARIABLE_NAME, value = VALUE, fill = 0)
out_long <- gather(out_wide, key = VARIABLE_NAME, value = VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS)

#check sampling within each year at each plot
tapply(out_long$VARIABLE_NAME, list(out_long$SITE_ID, out_long$DATE), en) 
tapply(out_long$SITE_ID, out_long$DATE, en) 

# Write CSV file for cleaned data (L3)
write.csv(out_long, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-cap-herps-banville.csv", row.names = F)

