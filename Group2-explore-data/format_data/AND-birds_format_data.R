library(tidyverse)

#Nathan's original method reads in data from his local computer
#setwd("LTER-Data/AND-birds/")
#data <- read.csv(file = "SA02402_v1.csv")


##########################
#read in data from EDI

# Package ID: knb-lter-and.4781.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Forest-wide bird survey at 183 sample sites the Andrews Experimental Forest from 2009-present.
# Data set creator:    - Andrews Forest LTER Site 
# Data set creator:  Sarah Hadley -  
# Metadata Provider:  Sarah Hadley -  
# Contact:    - Information Manager   - hjaweb@fsl.orst.edu
# Contact:  Sarah Hadley -    - sarah.frey@oregonstate.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 


               
#THIS FILE CONTAINS THE OBSERVATION DATA 
infile2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4781/2/3f989833925816bf1dfc8cf7844afed4" 
infile2 <- sub("^https","http",infile2) 
data <-read.csv(infile2,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "DBCODE",     
                    "ENTITY",     
                    "YEAR",     
                    "PLOT",     
                    "REPLICATE",     
                    "SURVEY_DATE",     
                    "RECORD",     
                    "PERIOD",     
                    "MINUTE",     
                    "SPECIES",     
                    "SEX",     
                    "DET_METH1",     
                    "DET_METH2",     
                    "DISTANCE",     
                    "NEW_RECORD",     
                    "COUNTER_SING",     
                    "ALT_SONG",     
                    "COMMENTS"    ), check.names=TRUE, stringsAsFactors = FALSE)
               
  
#work around to read in data from Google Drive if working offline:
data <- read.csv("~/Google Drive/LTER Metacommunities/LTER-DATA/L0-raw/AND-birds/archive_knb-lter-and/SA02402.csv")


str(data)
unique(cbind(data$PLOT, data$REPLICATE)) #this doesn't check whether sampling was equal in each year

en <- function(x) {length(unique(x))}
tapply(data$REPLICATE, list(data$PLOT, data$YEAR), en) #2014-2016 sampled way less

out <- data.frame(OBSERVATION_TYPE = "", 
                  SITE_ID = "",
                  DATE = "",
                  VARIABLE_NAME = "",
                  VARIABLE_UNITS = "",
                  VALUE = "")

head(data)
dat1 <- data %>% group_by(YEAR, PLOT, SPECIES) %>% 
  summarize(VALUE = n())
out <- cbind.data.frame(OBSERVATION_TYPE = "TAXON_COUNT",
                        SITE_ID = dat1$PLOT,
                        DATE = dat1$YEAR,
                        VARIABLE_NAME = dat1$SPECIES,
                        VARIABLE_UNITS = "count",
                        VALUE = dat1$VALUE
)

#write directly to the L3 folder
write.csv(~/Google Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-and-birds-wisnoski.csv)
