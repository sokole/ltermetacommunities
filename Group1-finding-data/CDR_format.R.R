#Formatting the cedar creek data set================
setwd("C:/Users/Aldo/MEGA/Projects/LTER/")
library(dplyr)

# Useful function----------------------------------------------------
formatMeteoDate=function(x,format,separator){
  
  out=as.data.frame(matrix(unlist(strsplit(as.character(x),separator)),length(x),3,byrow=T))
  names(out)=c(format)
  for(i in 1:3) {out[,i]=as.numeric(as.character(out[,i]))}
  return(out)
  
}


# Read data=============================================================

# http://www.cedarcreek.umn.edu/research/data?EXPERIMENT_SEARCH=%25&search_words=&CORE_AREA=%25&RESEARCHERS_LNAME=&SEARCH=Search
# Climate Data
climateRaw=read.csv("Data/Cedar Creek/DailyClimateSummary.csv")

# Demographic data
plantBioCC=read.csv("Data/Cedar Creek/CedarCreekBiomass.csv")


# Format data===========================================================================

# Biomass data-----------------------------------------------------------------------------------------

# Remove nitrogen addition plots
ccDat <- subset(plantBioCC,NitrAdd==0)
ccDat <- select(ccDat,Year,Field,Species,Biomass)

# Avoid mistakes
# Lower case grouping factors (aviods "carex sp." vs "Carex sp." mistake)
ccDat[,"Species"] <- tolower(ccDat[,"Species"])
# Erase non-species records
ccDat <- subset(ccDat,Species != "miscellaneous litter")

# Data problems: 
# 1. Many species could be quantified at the genus level only
# 2. "Moss and likens", and "Moss and likens 2"
# 3. "tragopogon dubius (major)" type of records

# Take means of biomass by site ("Field") by species 
cc_by_site <- ccDat %>% 
              group_by(Year,Field,Species) %>% 
              summarise(Biomass=mean(Biomass,na.rm=T))

# Add variable units and observation type
cc_by_site <- mutate(cc_by_site, VARIABLE_UNITS = "g per m2",
                                 OBSERVATION_TYPE = "TAXON_BIOMASS")

# Format column names to required format
names(cc_by_site) <- c("DATE","SITE_ID","VARIABLE_NAME","VALUE",
                       "VARIABLE_UNITS","OBSERVATION_TYPE")


# Climate data -----------------------------------------------------------

# Select precipitation data
climateCC <- select(climateRaw,Date,Precip.inches.)
climateCC <- cbind(formatMeteoDate(climateCC$Date,format=c("month","day","Year"),"/"),climateCC)

# Two options for CURRENT growing season summaries (May-August,April-July)
gs1       <- subset(climateCC,month > 3 & month < 9)
# Precipitations
gs1Prec   <- summarise(group_by(gs1,Year),gs1Prec = sum(Precip.inches.))

# Format precpitation data
names(gs1Prec) <- c("DATE","VALUE")
# Add remaining columns
gs1Prec         <- mutate(gs1Prec,
                          SITE_ID=NA,VARIABLE_NAME="growing season precipitation",
                          VARIABLE_UNITS="inches",OBSERVATION_TYPE="ENV_VAR")
  

# latitude/longitude information -------------------------------------------
spatialLocation <- data.frame(
  OBSERVATION_TYPE=rep("SPATIAL_COORDINATE",8),
  SITE_ID=c(LETTERS[1:4],LETTERS[1:4]),
  DATE=rep(NA,8),
  VARIABLE_NAME=c(rep("latitude",4),rep("longitude",4)),
  VARIABLE_UNITS=rep("dec. degrees",8),
  VALUE=c(rep(45.4038890,4),rep(-93.2680560,4))
)


# STACK data on top of each other ========================================

# Order columns and stack data on top of each other 
cc_by_site <- select(as.data.frame(cc_by_site),OBSERVATION_TYPE,
                     SITE_ID,DATE,VARIABLE_NAME,
                     VARIABLE_UNITS,VALUE)

gs1Prec    <- select(gs1Prec,OBSERVATION_TYPE,
                     SITE_ID,DATE,VARIABLE_NAME,
                     VARIABLE_UNITS,VALUE)

cedar_creek_plants=rbind(spatialLocation,gs1Prec,cc_by_site)

write.csv(cedar_creek_plants,
          "C:/Users/Aldo/Documents/CODE/WG_ltermetacommunities/CDR-plant-biomass.csv",
          row.names = F)
