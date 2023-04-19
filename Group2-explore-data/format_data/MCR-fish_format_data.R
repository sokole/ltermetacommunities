### Cleaning MOOREA CORAL REEF (MCR) data

# --------------------------------------------------------------------------------------------------------------------------------

# Max Castorani
# 8 June 2017

## Data manipulation packages
library(dplyr)
library(tidyr)

source("Group2-explore-data/format_data/pull_data_gdrive_fun.R")

# --------------------------------------------------------------------------------------------------------------------------------

### MCR Fish Data ###

# Package ID: knb-lter-mcr.6.61 Cataloging System:https://pasta.edirepository.org.
# Data set title: MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Fishes, ongoing since 2005.
# Data set creator:    - Moorea Coral Reef LTER 
# Data set creator:  Andrew Brooks - Moorea Coral Reef LTER 
# Metadata Provider:    - Moorea Coral Reef LTER 
# Contact:    - Information Manager Moorea Coral Reef LTER  - mcrlter@msi.ucsb.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcr/6/61/ac2c7a859ce8595ec1339e8530b9ba50" 
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
                 "Start_time",     
                 "End_time",     
                 "Location",     
                 "Site",     
                 "Habitat",     
                 "Transect",     
                 "Transect_Width",     
                 "Taxonomy",     
                 "Family",     
                 "Fish_Count",     
                 "Total_Length",     
                 "Length_Anomaly",     
                 "Biomass",     
                 "Coarse_Trophic",     
                 "Fine_Trophic",     
                 "Cloud_Cover",     
                 "Wind_Velocity",     
                 "Sea_State",     
                 "Swell",     
                 "Visibility",     
                 "Surge",     
                 "Diver"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1Date<-as.Date(dt1$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1Date) == length(tmp1Date[!is.na(tmp1Date)])){dt1$Date <- tmp1Date } else {print("Date conversion failed for dt1$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1Date) 
if (class(dt1$Start_time)!="factor") dt1$Start_time<- as.factor(dt1$Start_time)
if (class(dt1$End_time)!="factor") dt1$End_time<- as.factor(dt1$End_time)
if (class(dt1$Location)!="factor") dt1$Location<- as.factor(dt1$Location)
if (class(dt1$Site)!="factor") dt1$Site<- as.factor(dt1$Site)
if (class(dt1$Habitat)!="factor") dt1$Habitat<- as.factor(dt1$Habitat)
if (class(dt1$Transect)!="factor") dt1$Transect<- as.factor(dt1$Transect)
if (class(dt1$Transect_Width)=="factor") dt1$Transect_Width <-as.numeric(levels(dt1$Transect_Width))[as.integer(dt1$Transect_Width) ]               
if (class(dt1$Transect_Width)=="character") dt1$Transect_Width <-as.numeric(dt1$Transect_Width)
if (class(dt1$Taxonomy)!="factor") dt1$Taxonomy<- as.factor(dt1$Taxonomy)
if (class(dt1$Family)!="factor") dt1$Family<- as.factor(dt1$Family)
if (class(dt1$Fish_Count)=="factor") dt1$Fish_Count <-as.numeric(levels(dt1$Fish_Count))[as.integer(dt1$Fish_Count) ]               
if (class(dt1$Fish_Count)=="character") dt1$Fish_Count <-as.numeric(dt1$Fish_Count)
if (class(dt1$Total_Length)=="factor") dt1$Total_Length <-as.numeric(levels(dt1$Total_Length))[as.integer(dt1$Total_Length) ]               
if (class(dt1$Total_Length)=="character") dt1$Total_Length <-as.numeric(dt1$Total_Length)
if (class(dt1$Length_Anomaly)!="factor") dt1$Length_Anomaly<- as.factor(dt1$Length_Anomaly)
if (class(dt1$Biomass)=="factor") dt1$Biomass <-as.numeric(levels(dt1$Biomass))[as.integer(dt1$Biomass) ]               
if (class(dt1$Biomass)=="character") dt1$Biomass <-as.numeric(dt1$Biomass)
if (class(dt1$Coarse_Trophic)!="factor") dt1$Coarse_Trophic<- as.factor(dt1$Coarse_Trophic)
if (class(dt1$Fine_Trophic)!="factor") dt1$Fine_Trophic<- as.factor(dt1$Fine_Trophic)
if (class(dt1$Cloud_Cover)=="factor") dt1$Cloud_Cover <-as.numeric(levels(dt1$Cloud_Cover))[as.integer(dt1$Cloud_Cover) ]               
if (class(dt1$Cloud_Cover)=="character") dt1$Cloud_Cover <-as.numeric(dt1$Cloud_Cover)
if (class(dt1$Swell)=="factor") dt1$Swell <-as.numeric(levels(dt1$Swell))[as.integer(dt1$Swell) ]               
if (class(dt1$Swell)=="character") dt1$Swell <-as.numeric(dt1$Swell)
if (class(dt1$Visibility)=="factor") dt1$Visibility <-as.numeric(levels(dt1$Visibility))[as.integer(dt1$Visibility) ]               
if (class(dt1$Visibility)=="character") dt1$Visibility <-as.numeric(dt1$Visibility)
if (class(dt1$Surge)=="factor") dt1$Surge <-as.numeric(levels(dt1$Surge))[as.integer(dt1$Surge) ]               
if (class(dt1$Surge)=="character") dt1$Surge <-as.numeric(dt1$Surge)
if (class(dt1$Diver)!="factor") dt1$Diver<- as.factor(dt1$Diver)

# Convert Missing Values to NA for non-dates

dt1$Taxonomy <- as.factor(ifelse((trimws(as.character(dt1$Taxonomy))==trimws("No fish observed")),NA,as.character(dt1$Taxonomy)))
dt1$Total_Length <- ifelse((trimws(as.character(dt1$Total_Length))==trimws("-1.0")),NA,dt1$Total_Length)               
suppressWarnings(dt1$Total_Length <- ifelse(!is.na(as.numeric("-1.0")) & (trimws(as.character(dt1$Total_Length))==as.character(as.numeric("-1.0"))),NA,dt1$Total_Length))
dt1$Length_Anomaly <- as.factor(ifelse((trimws(as.character(dt1$Length_Anomaly))==trimws("na")),NA,as.character(dt1$Length_Anomaly)))
dt1$Biomass <- ifelse((trimws(as.character(dt1$Biomass))==trimws("-1")),NA,dt1$Biomass)               
suppressWarnings(dt1$Biomass <- ifelse(!is.na(as.numeric("-1")) & (trimws(as.character(dt1$Biomass))==as.character(as.numeric("-1"))),NA,dt1$Biomass))
dt1$Biomass <- ifelse((trimws(as.character(dt1$Biomass))==trimws("######.#")),NA,dt1$Biomass)               
suppressWarnings(dt1$Biomass <- ifelse(!is.na(as.numeric("######.#")) & (trimws(as.character(dt1$Biomass))==as.character(as.numeric("######.#"))),NA,dt1$Biomass))
dt1$Coarse_Trophic <- as.factor(ifelse((trimws(as.character(dt1$Coarse_Trophic))==trimws("na")),NA,as.character(dt1$Coarse_Trophic)))
dt1$Fine_Trophic <- as.factor(ifelse((trimws(as.character(dt1$Fine_Trophic))==trimws("na")),NA,as.character(dt1$Fine_Trophic)))
dt1$Visibility <- ifelse((trimws(as.character(dt1$Visibility))==trimws("-1")),NA,dt1$Visibility)               
suppressWarnings(dt1$Visibility <- ifelse(!is.na(as.numeric("-1")) & (trimws(as.character(dt1$Visibility))==as.character(as.numeric("-1"))),NA,dt1$Visibility))


mcr.fish <- dt1 %>% as_tibble()

## Read in the data
# mcr.fish <- read_csv_gdrive("0BxUZSA1Gn1HZRUVMenVlUndsRnM") %>%
#   tbl_df()
# 
# #Google Drive File Stream method:
# mcr.fish <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/MCR-fish/MCR_LTER_Annual_Fish_Survey_20160509.csv", stringsAsFactors = FALSE)

# Replace underscores with dots for convenience. Also convert to lowercase.
colnames(mcr.fish) <- tolower(gsub("_", ".", colnames(mcr.fish)))

# Code species guild
mcr.fish <- mcr.fish %>%
  dplyr::rename(species = taxonomy,
                count = fish.count) %>%
  dplyr::filter(species != "No fish observed",        # Drop unneeded species codes
                !is.na(species),
                !is.na(count)) %>%
  droplevels() %>%
  # Convert date to year
  mutate(year = as.numeric(as.character(year))) %>%
  dplyr::select(-date)

#why are there extra sites?
unique(mcr.fish$site)
en <- function(x){length(unique(x))}
tapply(mcr.fish$year, list(mcr.fish$site), en)
tapply(mcr.fish$transect, list(mcr.fish$site, mcr.fish$habitat, mcr.fish$year), en)
#the extra sites appear to be three transects in the forereef habitats in 2015 only. All other habitats have four transects per site per year. Remove the sites with decimals.
mcr.fish <- mcr.fish %>%
  dplyr::filter(site != 0.50,              
                site != 2.50,
                site != 4.25,
                site != 4.75,
                site != 5.25,
                site != 5.75)




# For each species, average the abundance data by year, habitat, plot ('site'), and subplot ('transect')
mcr.fish_clean <- mcr.fish %>%
  group_by(year, site, habitat, transect, species) %>%
  dplyr::summarise(biomass = mean(biomass, na.rm = TRUE)) %>%
  ungroup() %>%
  droplevels()

# Fix habitat
mcr.fish_clean$habitat <- as.character(mcr.fish_clean$habitat)
mcr.fish_clean$habitat[mcr.fish_clean$habitat == "BA"] <- "Backreef"
mcr.fish_clean$habitat[mcr.fish_clean$habitat == "FO"] <- "Forereef"
mcr.fish_clean$habitat[mcr.fish_clean$habitat == "FR"] <- "Fringing"

# Convert from long to wide and back to long to be sure that we have fully propagated taxa
mcr.fish_clean_wide <- spread(mcr.fish_clean, key = species, value = biomass, fill = 0)
mcr.fish_clean_long <- gather(mcr.fish_clean_wide, key = species, value = biomass, -year, -site, -transect, -habitat)

# Finish cleaning data by renaming and adding columns
mcr.fish_clean <- mcr.fish_clean_long %>%
  mutate(project = "fish",  # rename what they called site to what we call project
         plot = site) %>%
  select(-site) %>%
  mutate(site = "mcr",   # format column names
         plot = paste0("location_", plot),
         subplot = paste0("transect_", transect),
         abundance = biomass,  # NOTE: We are using biomass density in dry grams (not ash-free biomass)
         unitAbund = "mean.biomass",
         scaleAbund = "250_m2",
         growth = NA,
         plot = as.factor(paste0("transect_", plot)),
         uniqueID = paste(site, project, plot, subplot, sep = "_"),
         guild = "fish") %>%
  select(year, site, habitat, project, plot, subplot, uniqueID, guild, species, abundance, unitAbund, scaleAbund) #, growth)


# Remove unneeded files
rm(mcr.fish, mcr.fish_clean_long, mcr.fish_clean_wide)

# --------------------------------------------------------------------------------------------------------------------------------

# Reformat column names
mcr.fish_reformat <- mcr.fish_clean %>%
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
# write.csv(mcr.fish_reformat, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L2-mcr-fish-castorani.csv", row.names = F)


# --------------------------------------------------------------------------------------------------------------------------------
# Aggregate by site, then add spatial information
mcr.fish_L3 <- mcr.fish_reformat %>%
  dplyr::group_by(SITE_ID, DATE, VARIABLE_NAME) %>%
  dplyr::summarise(OBSERVATION_TYPE = unique(OBSERVATION_TYPE),
                   VARIABLE_UNITS = unique(VARIABLE_UNITS),
                   VALUE = mean(VALUE, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE) %>%
  as.data.frame(.)

# Replace underscores with dots in location IDs for future plotting. 
mcr.fish_L3$SITE_ID <- gsub("_", "", mcr.fish_L3$SITE_ID)

spatial.coords <- data.frame(
  "OBSERVATION_TYPE" = rep("SPATIAL_COORDINATE", length(unique(mcr.fish_L3$SITE_ID))*2),
  "SITE_ID" = rep(unique(mcr.fish_L3$SITE_ID), times = 2),
  "DATE" = rep("NA", length(unique(mcr.fish_L3$SITE_ID))*2),
  "VARIABLE_NAME" = c(rep("LAT", length(unique(mcr.fish_L3$SITE_ID))),
                      rep("LONG", length(unique(mcr.fish_L3$SITE_ID)))
  ),
  "VARIABLE_UNITS" = rep("dec. degrees", length(unique(mcr.fish_L3$SITE_ID))*2),
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

mcr.fish_L3_final <- rbind(spatial.coords, mcr.fish_L3)

# Write CSV file for cleaned data (L3)
write.csv(mcr.fish_L3_final, file = "Manuscripts/MS3/data/L3_datasets/L3-mcr-fish-castorani.csv", row.names = F)
