### Cleaning MOOREA CORAL REEF (MCR) data

# --------------------------------------------------------------------------------------------------------------------------------

# Max Castorani
# 8 June 2017

## Data manipulation packages
library(dplyr)
library(tidyr)



### MCR Invertebrate Data ###
# Package ID: knb-lter-mcr.7.33 Cataloging System:https://pasta.edirepository.org.
# Data set title: MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Other Benthic Invertebrates, ongoing since 2005.
# Data set creator:    - Moorea Coral Reef LTER 
# Data set creator:  Robert Carpenter - Moorea Coral Reef LTER 
# Metadata Provider:    - Moorea Coral Reef LTER 
# Contact:    - Information Manager Moorea Coral Reef LTER  - mcrlter@msi.ucsb.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcr/7/33/668de777b44a8f656a19c3ca25b737c5" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Year",     
                 "Date",     
                 "Location",     
                 "Site",     
                 "Habitat",     
                 "Transect",     
                 "Quadrat",     
                 "Taxonomy",     
                 "Count"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Year)!="factor") dt1$Year<- as.factor(dt1$Year)                                   
# attempting to convert dt1$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1Date<-as.Date(dt1$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1Date) == length(tmp1Date[!is.na(tmp1Date)])){dt1$Date <- tmp1Date } else {print("Date conversion failed for dt1$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1Date) 
if (class(dt1$Location)!="factor") dt1$Location<- as.factor(dt1$Location)
if (class(dt1$Site)!="factor") dt1$Site<- as.factor(dt1$Site)
if (class(dt1$Habitat)!="factor") dt1$Habitat<- as.factor(dt1$Habitat)
if (class(dt1$Transect)!="factor") dt1$Transect<- as.factor(dt1$Transect)
if (class(dt1$Quadrat)!="factor") dt1$Quadrat<- as.factor(dt1$Quadrat)
if (class(dt1$Taxonomy)!="factor") dt1$Taxonomy<- as.factor(dt1$Taxonomy)
if (class(dt1$Count)=="factor") dt1$Count <-as.numeric(levels(dt1$Count))[as.integer(dt1$Count) ]               
if (class(dt1$Count)=="character") dt1$Count <-as.numeric(dt1$Count)



## Read in the data
mcr.inverts <- as_tibble(dt1)

# mcr.inverts <- read_csv_gdrive("0BxUZSA1Gn1HZU2hQdC0wVVNQdDA") %>%
#   tbl_df()
# 
# #Google Drive File Stream method:
# mcr.inverts <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/MCR-inverts/MCR_LTER_Annual_Survey_Herbiv_Invert_20150330.csv", stringsAsFactors = FALSE)



# Replace underscores with dots for convenience. Also convert to lowercase.
colnames(mcr.inverts) <- tolower(gsub("_", ".", colnames(mcr.inverts)))

# Code species guild
mcr.inverts <- mcr.inverts %>%
  dplyr::rename(species = taxonomy) %>%
  dplyr::filter(species != "No invertebrate observed",        # Drop unneeded species codes
                species != "Culcita novaeguineae (1m away)",  # Drop observations of taxa outside of the quadrat
                species != "Acanthaster planci (1m away)",
                species != "Tectus niloticus (1m away)",
                species != "Turbo marmoratus (1m away)",
                !is.na(species),
                !is.na(count)) %>%
  droplevels() %>%
  # Convert date to year
  mutate(year = as.numeric(strtrim(as.character(date), 4))) %>%
  dplyr::select(-date)

# For each species, average the abundance data by year, habitat, plot ('site'), and subplot ('transect')
mcr.inverts_clean <- mcr.inverts %>%
  group_by(year, site, habitat, transect, species) %>%
  dplyr::summarise(count = mean(count, na.rm = TRUE)) %>%
  ungroup() %>%
  droplevels() 

# Convert from long to wide and back to long to be sure that we have fully propagated taxa
mcr.inverts_clean_wide <- spread(mcr.inverts_clean, key = species, value = count, fill = 0)
mcr.inverts_clean_long <- gather(mcr.inverts_clean_wide, key = species, value = count, -year, -site, -transect, -habitat)

# Finish cleaning data by renaming and adding columns
mcr.inverts_clean <- mcr.inverts_clean_long %>%
  mutate(project = "inverts",  # rename what they called site to what we call project
         plot = site) %>%
  select(-site) %>%
  mutate(site = "mcr",   # format column names
         plot = paste0("location_", sapply(strsplit(as.character(plot), " "), tail, 1)),
         subplot = paste0("transect_", transect),
         abundance = count, 
         unitAbund = "mean.count",
         scaleAbund = "1_m2",
         growth = NA,
         uniqueID = paste(site, project, plot, subplot, sep = "_"),
         guild = "inverts") %>%
  select(year, site, habitat, project, plot, subplot, uniqueID, guild, species, abundance, unitAbund, scaleAbund) #, growth)

# Remove unneeded files
rm(mcr.inverts, mcr.inverts_clean_long, mcr.inverts_clean_wide)

# --------------------------------------------------------------------------------------------------------------------------------

# Reformat column names
mcr.inverts_reformat <- mcr.inverts_clean %>%
  dplyr::mutate(OBSERVATION_TYPE = "TAXON_COUNT",
                VARIABLE_UNITS = paste0(unitAbund, ".per.", scaleAbund),
                UNIQUE_SPATIAL_ID = paste(plot, habitat, subplot, sep = "_")) %>%
  dplyr::rename(VALUE = abundance,
                VARIABLE_NAME = species,
                DATE = year,
                SITE_ID = plot,
                HABITAT = habitat,
                SUB_SITE_ID = subplot) %>%
  dplyr::select(OBSERVATION_TYPE,
                SITE_ID, 
                HABITAT, 
                SUB_SITE_ID, 
                UNIQUE_SPATIAL_ID,
                DATE,
                VARIABLE_NAME,
                VARIABLE_UNITS,
                VALUE)

# Write CSV file for cleaned data (L2. Skipping L1 because data are already aggregated by year)
# write.csv(mcr.inverts_reformat, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L2-mcr-inverts-castorani.csv", row.names = F)


# --------------------------------------------------------------------------------------------------------------------------------
# Aggregate by site, then add spatial information
mcr.inverts_L3 <- mcr.inverts_reformat %>%
  dplyr::group_by(SITE_ID, DATE, VARIABLE_NAME) %>%
  dplyr::summarise(OBSERVATION_TYPE = unique(OBSERVATION_TYPE),
                   VARIABLE_UNITS = unique(VARIABLE_UNITS),
                   VALUE = mean(VALUE, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE) %>%
  as.data.frame(.)

# Replace underscores with dots in location IDs for future plotting. 
mcr.inverts_L3$SITE_ID <- gsub("_", "", mcr.inverts_L3$SITE_ID)

spatial.coords <- data.frame(
  "OBSERVATION_TYPE" = rep("SPATIAL_COORDINATE", length(unique(mcr.inverts_L3$SITE_ID))*2),
  "SITE_ID" = rep(unique(mcr.inverts_L3$SITE_ID), times = 2),
  "DATE" = rep("NA", length(unique(mcr.inverts_L3$SITE_ID))*2),
  "VARIABLE_NAME" = c(rep("LAT", length(unique(mcr.inverts_L3$SITE_ID))),
                      rep("LONG", length(unique(mcr.inverts_L3$SITE_ID)))
  ),
  "VARIABLE_UNITS" = rep("dec. degrees", length(unique(mcr.inverts_L3$SITE_ID))*2),
  "VALUE" = c(-17.47913579,
              -17.47354064,
              -17.51234592,
              -17.54184642,
              -17.58000273,
              -17.51787861,
              -149.8377064,
              -149.8039267,
              -149.7614294,
              -149.7669862,
              -149.8715382,
              -149.9230353)
) %>%
  dplyr::mutate(OBSERVATION_TYPE = as.character(OBSERVATION_TYPE),
                DATE = as.numeric(DATE),
                VARIABLE_NAME = as.character(VARIABLE_NAME),
                VARIABLE_UNITS = as.character(VARIABLE_UNITS)) %>%
  dplyr::mutate(DATE = NA)

mcr.inverts_L3_final <- rbind(spatial.coords, mcr.inverts_L3)

# Write CSV file for cleaned data (L3)
write.csv(mcr.inverts_L3_final, file = "Manuscripts/MS3/data/L3_datasets/L3-mcr-inverts-castorani.csv", row.names = F)
