###libraries####
library(ggplot2)
library(lattice)
library(car)
library(reshape2)
library(reshape)
#library(TSA)
library(plyr)
library(dplyr)
#library(visreg)
#library(MASS)
#library(modEvA) 
#library(BiodiversityR)
#library(gridExtra)
#library(AICcmodavg)
#library(nlme)
#library(mgcv)
#library(lme4)
#library(VTrack)
#library(igraph)
#library(MARSS)
library(splitstackshape) ###package to use cSplit and do text to column like excel
#library(chron)
#library(rgdal)#package for geospatial data
#library(RInSp)#package for intraspecific niche variation
#library(boot)#package for boostrapping operations

#library(cowplot)

# Check for and install required packages
#library()

for (package in c('splitstackshape', 'dplyr', 'reshape', 'reshape2', 'car', 'ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
} 

# 


#setwd("E:/metacommunity_papers")

####Data preparation####
#Data - CPUE of fish collected via electrofishing
#data obtained from Jen Rahage and are not available on EDI. See citation for knb id with metadata.
fish.raw<-read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/FCE-fish-Rehage/fce_fish_rehage_raw.csv")

#Eliminating the sites that could not be sampled due to issues in the field
fish.raw<-subset(fish.raw, Sample %in% c("Yes"))

###
#Selecting main columns - excluding environmental data and concentrating on CPUE
###

fish.raw2<-fish.raw[,c(1:63, 66)]
fish.raw2$DATE2<-fish.raw2$DATE

#separate DATE column to get year, day, month separetly in columns
fish.raw2<-cSplit(fish.raw2, "DATE2", sep = "/", type.convert = FALSE)
colnames(fish.raw2)[65:67]<-c("MONTH", "DAY", "YEAR")
fish.raw2<-as.data.frame(fish.raw2)
#getting year and excluding day/month
fish.raw2<-fish.raw2[,c(1:64, 67)]
fish.raw2$YEAR<-as.numeric(fish.raw2$YEAR)

# fish.cpue<-melt(fish.raw2, id.vars = ("ID", "DATE","YEAR","SEASON", "RIVER", "CREEK", "SITETYPE", "BOUT", "DISTANCE"))
#Making long format for species
fish.cpue<-melt(fish.raw2, id.vars = c(1:7, 64:65))

#standardizing CPUE per bout distance
fish.cpue<-transform(fish.cpue, CPUE100M = (value/DISTANCE)*100)


###
#Aggregating data per Creek_River combination - fish CPUE aggregated as the sum (i.e., total CPUE) of the bouts (replicates)
###
fish.agg<-ddply(fish.cpue, c("YEAR", "SEASON","CREEK", "RIVER", "variable"), summarise,
                sum.CPUE = sum(CPUE100M))

#merging creek and river as SITE_ID identifier
fish.agg<-within(fish.agg, SITE_ID <- paste(CREEK, RIVER, sep='_'))

fish.agg1<-subset(fish.agg, select = c("SITE_ID", "YEAR", "SEASON", "variable", "sum.CPUE"))
colnames(fish.agg1)<-c("SITE_ID", "DATE", "SEASON", "VARIABLE_NAME", "VALUE")

VARIABLE_UNITS<-"CPUE = (COUNT / DISTANCE (M))*100"
OBSERVATION_TYPE<-"TAXON_COUNT"

fish.agg1<-as.data.frame(cbind(fish.agg1, VARIABLE_UNITS, OBSERVATION_TYPE))
fish.agg1$VARIABLE_NAME<-as.character(fish.agg1$VARIABLE_NAME)
fish.agg1$VARIABLE_UNITS<-as.character(fish.agg1$VARIABLE_UNITS)
fish.agg1$OBSERVATION_TYPE<-as.character(fish.agg1$OBSERVATION_TYPE)

#Version with CPUE | Season as columns
fish.agg2<-dcast(fish.agg1, SITE_ID + DATE + VARIABLE_UNITS + VARIABLE_NAME + OBSERVATION_TYPE ~ SEASON, value.var = "VALUE")
colnames(fish.agg2)[6:8]<-c("DRY", "TRANS", "WET")
#Adding the average of sum (total) cpue per season
fish.agg2<-transform(fish.agg2, MEAN.VALUE = rowMeans(fish.agg2[,6:8], na.rm = TRUE))


#fish.agg2 with functional classfication for species#this is a CSV from ROlando containing functional groups for each fish species
sp.function<-read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/FCE-fish-Rehage/species_function1.csv")
fish.agg3<-merge(fish.agg2, sp.function, by = "VARIABLE_NAME", all.x = TRUE)


#####Excluding 2004 and 2017, and only considering dry season with complete observations#################
##After the first examination round we decided to eliminate as well 2005 and 2011 since observations
##were incomplete across the sites. Also, for dry season TB sites were excluded due to incomplete temporal representation

fish.dry<-subset(fish.agg2, DATE != 2004)%>%
  subset(., DATE != 2005)%>%
  subset(., DATE != 2011)%>%
  subset(., !SITE_ID %in% c("1_TB", "2_TB", "3_TB", "4_TB"))%>%
  subset(., select = c(-TRANS, -WET, -MEAN.VALUE))%>%
  subset(., DRY != "NA")

colnames(fish.dry)[6]<-c("VALUE")

fish.wet<-subset(fish.agg2, !DATE %in% c(2004:2010))%>%
  # subset(., DATE != 2017)%>%
  # subset(., DATE != 2011)%>%
  # subset(., !SITE_ID %in% c("1_TB", "2_TB", "3_TB", "4_TB"))%>%
  subset(., select = c(-TRANS, -DRY, -MEAN.VALUE))%>%
  subset(., WET != "NA")
colnames(fish.wet)[6]<-c("VALUE")

######################################################################################
#Selecting environmental variables
###

envi.raw2<-fish.raw[,c(1:7, 68:71)]
envi.raw2$DATE2<-envi.raw2$DATE

#separate DATE column to get year, day, month separetly in columns
envi.raw2<-cSplit(envi.raw2, "DATE2", sep = "/", type.convert = FALSE)
colnames(envi.raw2)[12:14]<-c("MONTH", "DAY", "YEAR")
envi.raw2<-as.data.frame(envi.raw2)
#getting year and excluding day/month
envi.raw2<-envi.raw2[,c(1:11, 14)]

envi.l<-melt(envi.raw2, id.vars = c(1:7, 12))


###
#Aggregating data per Creek_River combination - Envi aggregated as the mean of the bouts (replicates)
###


envi.agg<-ddply(envi.l, c("YEAR", "SEASON","CREEK", "RIVER", "variable"), summarise,
                mean.envi = mean(value))

#merging creek and river as SITE_ID identifier
envi.agg<-within(envi.agg, SITE_ID <- paste(CREEK, RIVER, sep='_'))

envi.agg1<-subset(envi.agg, select = c("SITE_ID", "YEAR", "SEASON", "variable", "mean.envi"))
colnames(envi.agg1)<-c("SITE_ID", "DATE", "SEASON", "VARIABLE_NAME", "VALUE")

VARIABLE_UNITS <- as.data.frame(rbind(c("DEPTH", "meter"), c("TEMP", "celsius"), c("DOMG", "Mg/L"), c("SALINITY", "ppt")))
colnames(VARIABLE_UNITS)<-c("VARIABLE_NAME", "VARIABLE_UNITS")

envi.agg1<-merge(envi.agg1, VARIABLE_UNITS, by = "VARIABLE_NAME", all.x = TRUE)
envi.agg1$VARIABLE_NAME<-factor(envi.agg1$VARIABLE_NAME)
levels(envi.agg1$VARIABLE_NAME)<-list("DEPTH" = c("DEPTH"), "TEMPERATURE" = c("TEMP"),
                                      "DISSOLVED_OXYGEN" = c("DOMG"), "SALINITY" = "SALINITY")

OBSERVATION_TYPE<-"ENV_VAR"
envi.agg1<-as.data.frame(cbind(envi.agg1, OBSERVATION_TYPE))

envi.agg1$VARIABLE_NAME<-as.character(envi.agg1$VARIABLE_NAME)
envi.agg1$VARIABLE_UNITS<-as.character(envi.agg1$VARIABLE_UNITS)
envi.agg1$OBSERVATION_TYPE<-as.character(envi.agg1$OBSERVATION_TYPE)


#Version with ENVI | Season as columns
envi.agg2<-dcast(envi.agg1, SITE_ID + DATE + VARIABLE_UNITS + VARIABLE_NAME + OBSERVATION_TYPE ~ SEASON, value.var = "VALUE")
colnames(envi.agg2)[6:8]<-c("DRY", "TRANS", "WET")
#Adding the average of sum (total) cpue per season
envi.agg2<-transform(envi.agg2, MEAN.VALUE = rowMeans(fish.agg2[,6:8], na.rm = TRUE))

envi.dry<-subset(envi.agg2, select = c(-TRANS, -WET, -MEAN.VALUE))%>%subset(., DRY != "NA")
colnames(envi.dry)[6]<-c("VALUE")

envi.wet<-subset(envi.agg2, select = c(-TRANS, -DRY, -MEAN.VALUE))%>%subset(., WET != "NA")
colnames(envi.wet)[6]<-c("VALUE")

final.fce.fish.dry <- as.data.frame(rbind(fish.dry, envi.dry))
final.fce.fish.wet <- as.data.frame(rbind(fish.wet, envi.wet))

write.csv(final.fce.fish.dry, "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-fce-fish-rehageDry.csv")
write.csv(final.fce.fish.wet, "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-fce-fish-rehageWet.csv")










