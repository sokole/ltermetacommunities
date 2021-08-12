# Package ID: knb-lter-sbc.50.7 Cataloging System:https://pasta.edirepository.org.
# Data set title: SBC LTER: Reef: Annual time series of biomass for kelp forest species, ongoing since 2000.
# Data set creator:    - Santa Barbara Coastal LTER 
# Data set creator:  Daniel C Reed -  
# Contact:    - Information Manager, Santa Barbara Coastal LTER   - sbclter@msi.ucsb.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/50/7/24d18d9ebe4f6e8b94e222840096963c" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "YEAR",     
                 "MONTH",     
                 "DATE",     
                 "SITE",     
                 "TRANSECT",     
                 "SP_CODE",     
                 "PERCENT_COVER",     
                 "DENSITY",     
                 "WM_GM2",     
                 "DM_GM2",     
                 "SFDM",     
                 "AFDM",     
                 "SCIENTIFIC_NAME",     
                 "COMMON_NAME",     
                 "TAXON_KINGDOM",     
                 "TAXON_PHYLUM",     
                 "TAXON_CLASS",     
                 "TAXON_ORDER",     
                 "TAXON_FAMILY",     
                 "TAXON_GENUS",     
                 "GROUP",     
                 "MOBILITY",     
                 "GROWTH_MORPH",     
                 "COARSE_GROUPING"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DATE) == length(tmp1DATE[!is.na(tmp1DATE)])){dt1$DATE <- tmp1DATE } else {print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DATE) 
if (class(dt1$SITE)!="factor") dt1$SITE<- as.factor(dt1$SITE)
if (class(dt1$TRANSECT)!="factor") dt1$TRANSECT<- as.factor(dt1$TRANSECT)
if (class(dt1$SP_CODE)!="factor") dt1$SP_CODE<- as.factor(dt1$SP_CODE)
if (class(dt1$PERCENT_COVER)=="factor") dt1$PERCENT_COVER <-as.numeric(levels(dt1$PERCENT_COVER))[as.integer(dt1$PERCENT_COVER) ]               
if (class(dt1$PERCENT_COVER)=="character") dt1$PERCENT_COVER <-as.numeric(dt1$PERCENT_COVER)
if (class(dt1$DENSITY)=="factor") dt1$DENSITY <-as.numeric(levels(dt1$DENSITY))[as.integer(dt1$DENSITY) ]               
if (class(dt1$DENSITY)=="character") dt1$DENSITY <-as.numeric(dt1$DENSITY)
if (class(dt1$WM_GM2)=="factor") dt1$WM_GM2 <-as.numeric(levels(dt1$WM_GM2))[as.integer(dt1$WM_GM2) ]               
if (class(dt1$WM_GM2)=="character") dt1$WM_GM2 <-as.numeric(dt1$WM_GM2)
if (class(dt1$DM_GM2)=="factor") dt1$DM_GM2 <-as.numeric(levels(dt1$DM_GM2))[as.integer(dt1$DM_GM2) ]               
if (class(dt1$DM_GM2)=="character") dt1$DM_GM2 <-as.numeric(dt1$DM_GM2)
if (class(dt1$SFDM)=="factor") dt1$SFDM <-as.numeric(levels(dt1$SFDM))[as.integer(dt1$SFDM) ]               
if (class(dt1$SFDM)=="character") dt1$SFDM <-as.numeric(dt1$SFDM)
if (class(dt1$AFDM)=="factor") dt1$AFDM <-as.numeric(levels(dt1$AFDM))[as.integer(dt1$AFDM) ]               
if (class(dt1$AFDM)=="character") dt1$AFDM <-as.numeric(dt1$AFDM)
if (class(dt1$SCIENTIFIC_NAME)!="factor") dt1$SCIENTIFIC_NAME<- as.factor(dt1$SCIENTIFIC_NAME)
if (class(dt1$COMMON_NAME)!="factor") dt1$COMMON_NAME<- as.factor(dt1$COMMON_NAME)
if (class(dt1$TAXON_KINGDOM)!="factor") dt1$TAXON_KINGDOM<- as.factor(dt1$TAXON_KINGDOM)
if (class(dt1$TAXON_PHYLUM)!="factor") dt1$TAXON_PHYLUM<- as.factor(dt1$TAXON_PHYLUM)
if (class(dt1$TAXON_CLASS)!="factor") dt1$TAXON_CLASS<- as.factor(dt1$TAXON_CLASS)
if (class(dt1$TAXON_ORDER)!="factor") dt1$TAXON_ORDER<- as.factor(dt1$TAXON_ORDER)
if (class(dt1$TAXON_FAMILY)!="factor") dt1$TAXON_FAMILY<- as.factor(dt1$TAXON_FAMILY)
if (class(dt1$TAXON_GENUS)!="factor") dt1$TAXON_GENUS<- as.factor(dt1$TAXON_GENUS)
if (class(dt1$GROUP)!="factor") dt1$GROUP<- as.factor(dt1$GROUP)
if (class(dt1$MOBILITY)!="factor") dt1$MOBILITY<- as.factor(dt1$MOBILITY)
if (class(dt1$GROWTH_MORPH)!="factor") dt1$GROWTH_MORPH<- as.factor(dt1$GROWTH_MORPH)
if (class(dt1$COARSE_GROUPING)!="factor") dt1$COARSE_GROUPING<- as.factor(dt1$COARSE_GROUPING)

# Convert Missing Values to NA for non-dates

dt1$PERCENT_COVER <- ifelse((trimws(as.character(dt1$PERCENT_COVER))==trimws("-99999")),NA,dt1$PERCENT_COVER)               
suppressWarnings(dt1$PERCENT_COVER <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$PERCENT_COVER))==as.character(as.numeric("-99999"))),NA,dt1$PERCENT_COVER))
dt1$DENSITY <- ifelse((trimws(as.character(dt1$DENSITY))==trimws("-99999")),NA,dt1$DENSITY)               
suppressWarnings(dt1$DENSITY <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$DENSITY))==as.character(as.numeric("-99999"))),NA,dt1$DENSITY))
dt1$WM_GM2 <- ifelse((trimws(as.character(dt1$WM_GM2))==trimws("-99999")),NA,dt1$WM_GM2)               
suppressWarnings(dt1$WM_GM2 <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$WM_GM2))==as.character(as.numeric("-99999"))),NA,dt1$WM_GM2))
dt1$DM_GM2 <- ifelse((trimws(as.character(dt1$DM_GM2))==trimws("-99999")),NA,dt1$DM_GM2)               
suppressWarnings(dt1$DM_GM2 <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$DM_GM2))==as.character(as.numeric("-99999"))),NA,dt1$DM_GM2))
dt1$SFDM <- ifelse((trimws(as.character(dt1$SFDM))==trimws("-99999")),NA,dt1$SFDM)               
suppressWarnings(dt1$SFDM <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$SFDM))==as.character(as.numeric("-99999"))),NA,dt1$SFDM))
dt1$AFDM <- ifelse((trimws(as.character(dt1$AFDM))==trimws("-99999")),NA,dt1$AFDM)               
suppressWarnings(dt1$AFDM <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$AFDM))==as.character(as.numeric("-99999"))),NA,dt1$AFDM))
dt1$SCIENTIFIC_NAME <- as.factor(ifelse((trimws(as.character(dt1$SCIENTIFIC_NAME))==trimws("-99999")),NA,as.character(dt1$SCIENTIFIC_NAME)))
dt1$TAXON_PHYLUM <- as.factor(ifelse((trimws(as.character(dt1$TAXON_PHYLUM))==trimws("-99999")),NA,as.character(dt1$TAXON_PHYLUM)))
dt1$TAXON_CLASS <- as.factor(ifelse((trimws(as.character(dt1$TAXON_CLASS))==trimws("-99999")),NA,as.character(dt1$TAXON_CLASS)))
dt1$TAXON_ORDER <- as.factor(ifelse((trimws(as.character(dt1$TAXON_ORDER))==trimws("-99999")),NA,as.character(dt1$TAXON_ORDER)))
dt1$TAXON_FAMILY <- as.factor(ifelse((trimws(as.character(dt1$TAXON_FAMILY))==trimws("-99999")),NA,as.character(dt1$TAXON_FAMILY)))


# Rename dataset
sbc.data <- dt1
