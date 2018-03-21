#convert ecocommDP package to ltermetacommunities format:
#NKL 03/20/2018

#1. read in flat tables from EDI
# Package ID: edi.124.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: Plant aboveground biomass data: BAC: Biodiversity and Climate.
# Data set creator:  David Tilman -  
# Metadata Provider:    - Cedar Creek LTER 
# Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
# Contact:  Dan Bahauddin - Information Manager Cedar Creek Ecosystem Science Reserve  - webmaster@cedarcreek.umn.edu
# Contact:  Colin Smith -  Environmental Data Initiative  - info@environmentaldatainitiative.org
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

#Observation table
infile1  <- "https://pasta.lternet.edu/package/data/eml/edi/124/3/067643a7b5e3de0fc79c035dcc80ae6a" 
infile1 <- sub("^https","http",infile1) 
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep="\t"  
                ,quot='"' 
        , col.names=c(
                    "record_id",     
                    "observation_id",     
                    "package_id",     
                    "sampling_location_id",     
                    "observation_datetime",     
                    "taxon_id",     
                    "variable_name",     
                    "value",     
                    "unit"    ), check.names=TRUE,
                    stringsAsFactors = F)
               
  
#Sampling location ancillary table
infile2  <- "https://pasta.lternet.edu/package/data/eml/edi/124/3/1591fa653f504d527aecea8600f4b03a" 
infile2 <- sub("^https","http",infile2) 
 dt2 <-read.csv(infile2,header=F 
          ,skip=1
            ,sep="\t"  
                ,quot='"' 
        , col.names=c(
                    "sampling_location_ancillary_id",     
                    "sampling_location_id",     
                    "datetime",     
                    "variable_name",     
                    "value"    ), check.names=TRUE,
                    stringsAsFactors = F)
               
  
        
#dataset summary table
infile3  <- "https://pasta.lternet.edu/package/data/eml/edi/124/3/35e680b74d58817276e416d4de63f447" 
infile3 <- sub("^https","http",infile3) 
 dt3 <-read.csv(infile3,header=F 
          ,skip=1
            ,sep="\t"  
                ,quot='"' 
        , col.names=c(
                    "package_id",     
                    "original_package_id",     
                    "length_of_survey_years",     
                    "number_of_years_sampled",     
                    "std_dev_interval_betw_years",     
                    "max_num_taxa",     
                    "geo_extent_bounding_box_m2"    ), check.names=TRUE,
                    stringsAsFactors = F)
               
  
#sampling location table      
infile4  <- "https://pasta.lternet.edu/package/data/eml/edi/124/3/10df3a0e704315b8394da2cd00e789b4" 
infile4 <- sub("^https","http",infile4) 
 dt4 <-read.csv(infile4,header=F 
          ,skip=1
            ,sep="\t"  
                ,quot='"' 
        , col.names=c(
                    "sampling_location_id",     
                    "sampling_location_name",     
                    "latitude",     
                    "longitude",     
                    "parent_sampling_location_id"    ), check.names=TRUE,
                    stringsAsFactors = F)
               
  
#taxon table
infile5  <- "https://pasta.lternet.edu/package/data/eml/edi/124/3/a6608cdbcabf3d70031b4490c11e0ac0" 
infile5 <- sub("^https","http",infile5) 
 dt5 <-read.csv(infile5,header=F 
          ,skip=1
            ,sep="\t"  
                ,quot='"' 
        , col.names=c(
                    "taxon_id",     
                    "taxon_name"    ), check.names=TRUE,
                    stringsAsFactors = F)
               
              
#2 Make TAXON_COUNT from the Observation table (dt1)
#select the columns of interest
dat1 <- dt1[,c("sampling_location_id", "observation_datetime", "taxon_id","variable_name", "value")]
names(dat1) <- c("SITE_ID", "DATE", "VARIABLE_NAME", "VARIABLE_UNITS", "VALUE")
#make a column that indicates, essentially, which flat table this is.
dat1$OBSERVATION_TYPE <- rep("TAXON_COUNT", length(dat1$VALUE))
#reorder columns
dat1 <- dat1[,c("OBSERVATION_TYPE","SITE_ID", "DATE", "VARIABLE_NAME", "VARIABLE_UNITS", "VALUE")]
str(dat1)


#3 Make SPATIAL_COORDINATE from the sampling location table (dt4)
#select the columns of interest and convert to long 
SITE_ID <- c(dt4$sampling_location_id, dt4$sampling_location_id)
VALUE <- c(dt4$latitude, dt4$longitude)
VARIABLE_NAME <- c(rep("latitude", length(dt4$latitude)), rep("longitude", length(dt4$longitude)))
OBSERVATION_TYPE <- rep("SPATIAL_COORDINATE", length(VALUE))
VARIABLE_UNITS <- rep("degrees", length(VALUE))
DATE <- rep(NA, length(VALUE))
dat4 <- data.frame(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE, stringsAsFactors = F)
rm(SITE_ID, VALUE, VARIABLE_NAME, OBSERVATION_TYPE, VARIABLE_UNITS, DATE)

#4 Combine into one big table
dat <- rbind(dat1, dat4)
str(dat)

#5 perform a few checks against dataset summary table (dt3)
ifelse(length(unique(dat1$VARIABLE_NAME))==dt3$max_num_taxa,
  "OK: Number of taxa matches dataset summary table.",
  "ERROR:Number of taxa does not match dataset summary table.")

date <- as.POSIXlt(dat1$DATE, format = "%Y-%m-%d")
year <- as.numeric(format(date, "%Y"))
ifelse(length(unique(year))== dt3$number_of_years_sampled, 
  "OK: Number of years sampled matches dataset summary table.",
  "ERROR:Number of years sampled does not match dataset summary table.")

ifelse( max(year)-min(year) + 1 == dt3$length_of_survey_years, 
  "OK: Length of survey matches dataset summary table.",
  "ERROR:Length of survey does not match dataset summary table.")

