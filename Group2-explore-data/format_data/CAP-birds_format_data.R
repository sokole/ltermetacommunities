#################################################################################################
# Point-count bird censusing: long-term monitoring in central Arizona-Phoenix, ongoing since 2000
# RKA 05/15/2018                                    
#################################################################################################

rm(list = ls())
# Check for and install required packages
for (package in c('dplyr', 'tidyverse', 'tidyr')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, dependencies=TRUE, repos='http://cran.rstudio.com/')
    library(package, character.only=T)
  }
}

# Package ID: knb-lter-cap.46.15 Cataloging System:https://pasta.edirepository.org.
# Data set title: Point-count bird censusing: long-term monitoring of bird abundance and diversity in       central Arizona-Phoenix, ongoing since 2000.
# Data set creator:  Heather Bateman - Arizona State University, Polytechnic campus 
# Data set creator:  Dan Childers - Arizona State University 
# Data set creator:  Madhusudan Katti - Department of Forestry and Environmental Resources 
# Data set creator:  Eyal Shochat - Ben-Gurion University of the Negev 
# Data set creator:  Paige Warren - University of Massachusetts-Amherst 
# Metadata Provider:  Stevan Earl - Arizona State University 
# Contact:    - Data Manager Julie Ann Wrigley Global Institute of Sustainability, Arizona State University  - caplter.data@asu.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

#infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cap/46/15/1ac975d0d3272f2eabe68a66e9f908ad" 
#infile1 <- sub("^https","http",infile1) 
#dt1 <-read.csv(infile1,header=F 
#               ,skip=1
#               ,sep=","  
#               ,quot='"' 
#               , col.names=c(
#                 "site_code",     
#                 "location_type",     
#                 "survey_date",     
#                 "time_start",     
#                 "time_end",     
#                 "observer_name_part",     
#                 "wind_speed",     
#                 "wind_dir",     
#                 "air_temp",     
#                 "cloud_cover",     
#                 "survey_notes",     
#                 "human_activity_notes",     
#                 "wind",     
#                 "precipitation",     
#                 "disturbances",     
#                 "sight_obstruct",     
#                 "noise_level",     
#                 "site_condition",     
#                 "non_bird_species",     
#                 "code",     
#                 "common_name",     
#                 "distance",     
#                 "bird_count",     
#                 "observation_notes",     
#                 "seen",     
#                 "heard",     
#                 "direction",     
#                 "QCcomment"    ), check.names=TRUE, stringsAsFactors=F)



#alternately, use Google Drive File Stream:
dt1 <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/CAP-birds-Banville/archive_knb-lter-cap/46_core_birds_ee23527b9fad8b2ead1a6f0b4471ab1e.csv", stringsAsFactors=FALSE)  

# Looking at the input data frame
str(dt1) 
head(dt1)
unique(dt1$site_code) #sites of point counts
unique(dt1$location_type) #classification of sites (checked for different sampling among classifications)                        

#Changing column format 
dt1$location_type <- as.character(dt1$location_type) #change location type to character to subset
dt1$site_code <- as.character(dt1$site_code) #change site code to character to subset
dt1$code <- as.character(dt1$code) #change species code to character to subset

#Extract year
dt1$Date <- as.POSIXct(dt1$survey_date)
dt1$Year <- as.numeric(format(dt1$Date, format = "%Y"))
dt1$Month <- as.numeric(format(dt1$Date, format = "%m"))
unique(dt1$Year) #number of years with observations (any number)
unique(dt1$Month)

#Check sampling within each year at each site classification
en <- function(x) {length(unique(x))} 
tapply(dt1$Date, list(dt1$location_type, dt1$Year), en) #SUBSET BY ESCA for 2000-onward dataset

#                     2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017
#DesertFertilization   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA    4   22
#ESCA                  45  141  135  110  111   82   60   66   60   64   71   70   67   79   75   76   72   47
#NDV                   NA   NA   NA    2   13    9    8    6    7    7    7    5    6    7   10    7    6   NA
#PASS                  NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA    8   42
#Riparian              20   88   67   21   82   64   46   42   44   42   49   50   47   49   50   53   44   NA
#SRBP                  NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   19   44   45   36   37

#Remove location types without consistent long-term sampling effort
dat <- dt1 %>% filter(location_type == "ESCA", #selecting subset of ESCA bird data (even sampling and design for 2001-onward)
                      Year != 2017,  #Drop 2017 until new data is updated in the file (2017 and 2018 data)
                      site_code != "M-9",
                      site_code != "V-18",
                      site_code != "X-8",
                      site_code != "V-16", #Drop sites M9, V18, V16, and X8 for uneven sampling design
                      ! grepl('Unidentified', common_name), #Remove unidentified species (~2% of total observations)
                      ! distance  %in% c(">40","FT" ), #Observations within 40m
                      ! is.na(distance), #Observations within 40m
                      Month == 3 | Month == 4 | Month == 5)  #Drop all but spring months for uneven sampling design
                     
#check sampling within each year at each plot
tapply(dat$Date, list(dat$site_code, dat$Year), en) 
tapply(dat$site_code, dat$Year, en) 

#select only points with complete cases
cc <- tapply(dat$Date, list(dat$site_code, dat$Year), en)
cc <- as.data.frame(cc)
cc$site_code <- row.names(cc)
cc$Code <- complete.cases(cc)
XX <- subset(cc, Code == "TRUE")
XX$Plot
data <- merge(dat, XX, by = "site_code", all.x = F, all.y=T)
data <- data %>% select(site_code,location_type,Date, Year,Month, distance,code,bird_count)

#calculate the maximum number of each species observed at each point in each year
out <- data.frame(OBSERVATION_TYPE = "", 
                  SITE_ID = "",
                  DATE = "",
                  VARIABLE_NAME = "",
                  VARIABLE_UNITS = "",
                  VALUE = "")

step1 <- data %>% group_by(Date, site_code, code) %>% 
  summarize(NUM = sum(bird_count))
step1$Year<- as.numeric(format(step1$Date, format = "%Y"))

step2 <- step1 %>% group_by(Year, site_code, code) %>% 
  summarize(VALUE = max(NUM))

out <- cbind.data.frame(OBSERVATION_TYPE = "TAXON_COUNT",
                        SITE_ID = step2$site_code,
                        DATE = step2$Year,
                        VARIABLE_NAME = step2$code,
                        VARIABLE_UNITS = "count",
                        VALUE = step2$VALUE)

# Convert from long to wide and back to long to be sure that we have fully propagated taxa
out_wide <- spread(out, key = VARIABLE_NAME, value = VALUE, fill = 0)
out_long <- gather(out_wide, key = VARIABLE_NAME, value = VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS)

#check sampling within each year at each plot
tapply(out_long$VARIABLE_NAME, list(out_long$SITE_ID, out_long$DATE), en) 
tapply(out_long$SITE_ID, out_long$DATE, en) 

# Write CSV file for cleaned data (L3)
write.csv(out_long, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-cap-birds-banville.csv", row.names = F)
