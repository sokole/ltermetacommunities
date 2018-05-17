#####################################################
# Hubbard Brook Valley Wide Point Count Bird Survey #
# NKL 05/05/2018                                    #
#####################################################

rm(list = ls())
library(dplyr)
library(tidyverse)

# Package ID: knb-lter-hbr.178.1 Cataloging System:https://pasta.lternet.edu.
# Data set title: Valleywide Bird Survey, Hubbard Brook Experimental Forest, 1999-present.
# Data set creator:  Nick Rodenhouse - Department of Biological Sciences, Wellesley College 
# Data set creator:  Scott Sillett - Migratory Bird Center, Smithsonian Conservation Biology Institute 
# Metadata Provider:    - Hubbard Brook Experimental Forest LTER 
# Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
# Contact:    - Information Manager, Hubbard Brook LTER   - hbr-im@lternet.edu
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-hbr.178.1
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

#infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/178/1/047f502e4c467e2b40491236ac189bbe" 
#infile1 <- sub("^https","http",infile1) 
# dt1 <-read.csv(infile1,header=F 
#          ,skip=1
#            ,sep="," 
#        , col.names=c(
#                    "Plot",     
#                    "Replicate",     
#                    "Date",     
#                    "Time",     
#                    "Observer",     
#                    "Sky",     
#                    "Wind",     
#                    "Bird.Number",     
#                    "Period",     
#                    "Minute",     
#                    "Species",     
#                    "Sex",     
#                    "Detection.Method",     
#                    "Distance",     
#                    "New.Record",     
#                    "Counter.sing",     
#                    "Comments"    ), check.names=TRUE, stringsAsFactors = #FALSE)
               
#alternately, use Google Drive File Stream:
dt1 <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/HBR-birds-Sillett/valleywide_bird.txt", stringsAsFactors=FALSE)  


#extract year
dt1$Date <- as.POSIXct(dt1$Date)
dt1$Year <- as.numeric(format(dt1$Date, format = "%Y"))
#re-code New.Record for years prior to 2006 (NA becomes 1 because they are equivalent)
dt1$New.Record <- ifelse(is.na(dt1$New.Record), 1, dt1$New.Record)

#check sampling within each year at each plot:
en <- function(x) {length(unique(x))} 
tapply(dt1$Date, list(dt1$Plot, dt1$Year), en)
tapply(dt1$Plot, dt1$Year, en) 
#1989 1999 2000 2001 2002 2005 2006 2007 2008 2009 2012 2013 
#   2   12    6  374  371  351  372  302    1  227  371  187 

#Remove years with little sampling 1989, 1999, 2000, 2008); remove records of unknown species; remove records that are not new records; select distances under 50m (Distance < 2)
dat <- dt1 %>%

  dplyr::filter(Year != 1989,         
                Year != 1999,
                Year != 2000,
                Year != 2008,
                Species != "UNKNOWN",
                Species != "UNK_MAMMAL",
                Species != "UNK_WOODPECKER",
                New.Record == 1,
                Distance < 2)    
               

#select only points with complete cases:
cc <- tapply(dat$Date, list(dat$Plot, dat$Year), en)
cc <- as.data.frame(cc)
cc$Plot <- row.names(cc)
cc$Code <- complete.cases(cc)
XX <- subset(cc, Code == "TRUE")
XX$Plot
data <- merge(dat, XX, by = "Plot", all.x = F, all.y=T)
data <- data[,1:18]

#look at sampling within each year again. Mostly 1-2 surveys, with 3 per season on 2006, 2007, and a few in 2013. Remove Replicate == 3 to balance out. Except that Replicate #3 is the only one in a few years...



tapply(data$Species, list(data$Plot, data$Year, data$Replicate), en)

#there are 1-3 sampling intervals per plot per year. Need equal effort at all plots in all years.
test <- data %>%
  dplyr::filter(Replicate < 2)    

tapply(data$Date, list(data$Plot, data$Year), en)


#calculate the maximum number of each species observed at each point in each year
out <- data.frame(OBSERVATION_TYPE = "", 
                  SITE_ID = "",
                  DATE = "",
                  VARIABLE_NAME = "",
                  VARIABLE_UNITS = "",
                  VALUE = "")

step1 <- data %>% group_by(Year, Plot, Species, Replicate) %>% 
  summarize(NUM = length(New.Record))

step2 <- step1 %>% group_by(Year, Plot, Species) %>% 
  summarize(VALUE = max(NUM))


out <- cbind.data.frame(OBSERVATION_TYPE = "TAXON_COUNT",
                        SITE_ID = step2$Plot,
                        DATE = step2$Year,
                        VARIABLE_NAME = step2$Species,
                        VARIABLE_UNITS = "count",
                        VALUE = step2$VALUE)



# Convert from long to wide and back to long to be sure that we have fully propagated taxa
out_wide <- spread(out, key = VARIABLE_NAME, value = VALUE, fill = 0)
out_long <- gather(out_wide, key = VARIABLE_NAME, value = VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS)

str(out_long)


# Write CSV file for cleaned data (L3)
write.csv(out_long, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-hbr-birds-sillett.csv", row.names = F)