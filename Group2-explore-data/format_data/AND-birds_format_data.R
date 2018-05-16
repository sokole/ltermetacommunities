library(tidyverse)


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
#infile2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4781/2/3f989833925816bf1dfc8cf7844afed4" 
#infile2 <- sub("^https","http",infile2) 
#data <-read.csv(infile2,header=F 
#          ,skip=1
#            ,sep=","  
#                ,quot='"' 
#        , col.names=c(
#                    "DBCODE",     
#                    "ENTITY",     
#                    "YEAR",     
#                    "PLOT",     
#                    "REPLICATE",     
#                    "SURVEY_DATE",     
#                    "RECORD",     
#                    "PERIOD",     
#                    "MINUTE",     
#                    "SPECIES",     
#                    "SEX",     
#                    "DET_METH1",     
#                    "DET_METH2",     
#                    "DISTANCE",     
#                    "NEW_RECORD",     
#                    "COUNTER_SING",     
#                    "ALT_SONG",     
#                    "COMMENTS"    ), check.names=TRUE, stringsAsFactors = FALSE)
               
  
#Google Drive File Stream alternative:
data <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/AND-birds/archive_knb-lter-and/SA02402.csv")

str(data)


en <- function(x) {length(unique(x))}
tapply(data$REPLICATE, list(data$PLOT, data$YEAR), en) #2014-2016 sampled way less
unique(data$SPECIES)

out <- data.frame(OBSERVATION_TYPE = "", 
                  SITE_ID = "",
                  DATE = "",
                  VARIABLE_NAME = "",
                  VARIABLE_UNITS = "",
                  VALUE = "")

head(data)

# take only new observations at nearest distance
# subset the first five years where sampling was high
dat1 <- data %>% group_by(YEAR, PLOT, RECORD, SPECIES) %>% 
  filter(NEW_RECORD == 1, DISTANCE == 1) %>% 
  filter(YEAR < 2014) %>% 
  summarize(count = n())

# take max count for each species across all 6 sampling periods
dat1 <- dat1 %>% group_by(YEAR, PLOT, SPECIES) %>% 
  summarize(VALUE = max(count))

# propogate the data
dat1 <- dat1 %>% spread(SPECIES, VALUE, fill = 0) %>% 
  gather(SPECIES, VALUE, -YEAR, -PLOT)

out <- cbind.data.frame(OBSERVATION_TYPE = as.character("TAXON_COUNT"),
                        SITE_ID = as.character(dat1$PLOT),
                        DATE = as.numeric(dat1$YEAR),
                        VARIABLE_NAME = as.character(dat1$SPECIES),
                        VARIABLE_UNITS = as.character("count"),
                        VALUE = as.numeric(dat1$VALUE)
)


#write directly to the L3 folder
write.csv(out, 
          file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-and-birds-wisnoski.csv",
          row.names = F)

# or via local google drive
write.csv(out, 
          file = "~/Google Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-and-birds-wisnoski.csv",
          row.names = F)
