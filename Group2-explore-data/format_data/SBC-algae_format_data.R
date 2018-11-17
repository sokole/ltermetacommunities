# --------------------------------------------------------- #
# Format raw data as a list of tables                       #
# SBC-algae-Lamy-Castorani                                  #
# Revised Nov 17, 2018 by MCN Castorani                     #
# --------------------------------------------------------- #

# Contributors: Riley Andrade, Max Castorani, Nina Lany, Sydne Record, Nicole Voelker

# Clear environment
rm(list = ls())

# Set your working environment to the GitHub repository, e.g.: 
#setwd("~/Documents/ltermetacommunities")

#Check to make sure working directory is correct
if(basename(getwd())!="ltermetacommunities"){cat("Plz change your working directory. It should be 'ltermetacommunities'")}

# Check for and install required packages
for (package in c('dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom', 'ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# ---------------------------------------------------------------------------------------------------
# IMPORT ***OLD DATA*** FROM GOOGLE DRIVE

# Assign data set of interest
# NOTE: Google Drive file ID is different for each dataset

# SBC LTER (Santa Barbara Coastal): Macroalgae
data.set <- "SBC-algae"
data.key <- "0BxUZSA1Gn1HZRUxaNmV1Y21abmc" # Google Drive file ID for file in ARCHIVE!!!

dat.long.old <-  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", data.key)) %>%
  dplyr::select(-X) # Remove column that contains rownames

# Get spatial coordinates
sbc.xy <- dat.long.old[dat.long.old$OBSERVATION_TYPE == "SPATIAL_COORDINATE", ] %>%
  droplevels()

# Get wave data
sbc.wave.dat <- dat.long.old[dat.long.old$VARIABLE_NAME == "WAVE_HT_MEAN" | 
                               dat.long.old$VARIABLE_NAME == "WAVE_HT_WINTER_MEAN" , ] %>%
  droplevels()

rm(dat.long.old)

# ---------------------------------------------------------------------------------------------------

# Import community data from EDI

# Package ID: knb-lter-sbc.50.6 Cataloging System:https://pasta.edirepository.org.
# Data set title: SBC LTER: Reef: Annual time series of biomass for kelp forest species, ongoing since 2000.
# Data set creator:    - Santa Barbara Coastal LTER 
# Data set creator:  Daniel C Reed -  
# Contact:    - Information Manager, Santa Barbara Coastal LTER   - sbclter@msi.ucsb.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/50/6/24d18d9ebe4f6e8b94e222840096963c" 
infile1 <- sub("^https","http",infile1) 
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
                 "GROWTH_MORPH"    ), check.names=TRUE)


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
if (class(dt1$DENSITY)=="factor") dt1$DENSITY <-as.numeric(levels(dt1$DENSITY))[as.integer(dt1$DENSITY) ]
if (class(dt1$WM_GM2)=="factor") dt1$WM_GM2 <-as.numeric(levels(dt1$WM_GM2))[as.integer(dt1$WM_GM2) ]
if (class(dt1$DM_GM2)=="factor") dt1$DM_GM2 <-as.numeric(levels(dt1$DM_GM2))[as.integer(dt1$DM_GM2) ]
if (class(dt1$SFDM)=="factor") dt1$SFDM <-as.numeric(levels(dt1$SFDM))[as.integer(dt1$SFDM) ]
if (class(dt1$AFDM)=="factor") dt1$AFDM <-as.numeric(levels(dt1$AFDM))[as.integer(dt1$AFDM) ]
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

comm.dat.unformatted <- dt1; rm(dt1)

# Recode "-99999" in unformatted community data
comm.dat.unformatted$PERCENT_COVER[comm.dat.unformatted$PERCENT_COVER < 0] <- NA
comm.dat.unformatted$DENSITY[comm.dat.unformatted$DENSITY < 0] <- NA
comm.dat.unformatted$WM_GM2[comm.dat.unformatted$WM_GM2 < 0] <- 0
comm.dat.unformatted$DM_GM2[comm.dat.unformatted$DM_GM2 < 0] <- 0
comm.dat.unformatted$SFDM[comm.dat.unformatted$SFDM < 0] <- 0
comm.dat.unformatted$AFDM[comm.dat.unformatted$AFDM < 0] <- 0
  
# ---------------------------------------------------------------------------------------------------

# Import temperature data from EDI

# Package ID: knb-lter-sbc.13.21 Cataloging System:https://pasta.edirepository.org.
# Data set title: SBC LTER: Reef: Bottom Temperature: Continuous water temperature, ongoing since 2000.
# Data set creator:    - Santa Barbara Coastal LTER 
# Data set creator:  Daniel C Reed -  
# Contact:    - Information Manager, Santa Barbara Coastal LTER   - sbclter@msi.ucsb.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/13/21/d707a45a2cd6eee1d016d99844d537da" 
infile1 <- sub("^https","http",infile1) 
dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Date",     
                 "Time",     
                 "temp_c",     
                 "serial",     
                 "site"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1Date<-as.Date(dt1$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1Date) == length(tmp1Date[!is.na(tmp1Date)])){dt1$Date <- tmp1Date } else {print("Date conversion failed for dt1$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1Date) 
if (class(dt1$temp_c)=="factor") dt1$temp_c <-as.numeric(levels(dt1$temp_c))[as.integer(dt1$temp_c) ]
if (class(dt1$serial)!="factor") dt1$serial<- as.factor(dt1$serial)
if (class(dt1$site)!="factor") dt1$site<- as.factor(dt1$site)

temp.dat.unformatted <- dt1; rm(dt1)
temp.dat.unformatted$YEAR <- as.numeric(format(temp.dat.unformatted$Date, "%Y"))

# ---------------------------------------------------------------------------------------------------
# Reformat environmental data (bottom water temperature)
temp.dat <- temp.dat.unformatted %>%
  dplyr::select(YEAR, temp_c, site) %>%
  dplyr::filter(site != "SCTW") %>%  # Remove two Channel Islands sites
  dplyr::filter(site != "SCDI") %>%
  group_by(YEAR, site) %>%
  dplyr::summarise(VALUE = mean(temp_c, na.omit = TRUE)) %>%
  ungroup() %>%
  dplyr::mutate(DATE = YEAR,
                VARIABLE_NAME = "TEMP_MEAN_C", 
                VARIABLE_UNITS = "degrees C",
                OBSERVATION_TYPE = "ENV_VAR",
                SITE_ID = site) %>%
  dplyr::select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE)

sbc.env <- temp.dat
rm(temp.dat, temp.dat.unformatted)

# ---------------------------------------------------------------------------------------------------
# Reformat community data
comm.dat.long <- comm.dat.unformatted %>%
  dplyr::select(YEAR, SITE, SP_CODE, DM_GM2, GROUP, MOBILITY) %>%  # Select variables of interest
  dplyr::filter(SITE != "SCTW") %>%  # Remove two Channel Islands sites
  dplyr::filter(SITE != "SCDI") %>%
  droplevels() %>%
  mutate(VARIABLE_NAME = SP_CODE,   # Reformat column names
         OBSERVATION_TYPE = "TAXON_COUNT",
         VALUE = DM_GM2,
         VARIABLE_UNITS = "g dry per m2",
         DATE = YEAR,
         SITE_ID = SITE,
         TAXON_GROUP = paste(MOBILITY, GROUP, sep = " ")) %>%
  dplyr::select(-DM_GM2, -SP_CODE, -YEAR, -SITE, -GROUP) %>%
  
  # These data are still at transect-level. Group by year, site, and taxon to calculate mean biomass
  dplyr::group_by(SITE_ID, DATE, VARIABLE_NAME) %>%
  dplyr::summarise(VALUE = mean(VALUE, na.omit = TRUE),
                   OBSERVATION_TYPE = unique(OBSERVATION_TYPE),
                   VARIABLE_UNITS = unique(VARIABLE_UNITS),
                   TAXON_GROUP = unique(TAXON_GROUP)) %>%
  ungroup() %>%
  dplyr::select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE, TAXON_GROUP)

  
comm.dat.long$TAXON_GROUP <- dplyr::recode(comm.dat.long$TAXON_GROUP, 
                                           `MOBILE FISH`  = "FISH",
                                           `SESSILE ALGAE` = "ALGAE")

comm.dat.long <- comm.dat.long[, c("OBSERVATION_TYPE", "SITE_ID", "DATE", "VARIABLE_NAME", "VARIABLE_UNITS", "VALUE", "TAXON_GROUP")] # Reorder columns

# # Spread from long to wide and then gather back to long to ensure equal observations across sites and time
# comm.dat.wide <- comm.dat.long %>%
#   dplyr::select(-TAXON_GROUP) %>%
#   spread(key = VARIABLE_NAME,  value = VALUE, fill = 0)

rm(comm.dat.unformatted)

# ---------------------------------------------------------------------------------------------------
# REMOVE YEAR 2000 BECAUSE WE ARE MISSING DATA
comm.dat.long <- comm.dat.long[comm.dat.long$DATE > 2000, ] %>%
  drop_na()  # Remove NA rows

# ---------------------------------------------------------------------------------------------------
# Put datasets together by group
unique(comm.dat.long$TAXON_GROUP)

# ALGAE (INCLUDING GIANT KELP)
sbc.algae.temp <- comm.dat.long %>%
  dplyr::filter(TAXON_GROUP == "ALGAE") %>%
  dplyr::select(-TAXON_GROUP)
  
sbc.algae.w.kelp <- rbind(sbc.xy, sbc.env, sbc.algae.temp); rm(sbc.algae.temp)       # Algae including giant kelp
sbc.algae.no.kelp <- sbc.algae.w.kelp[!sbc.algae.w.kelp$VARIABLE_NAME == "MAPY", ]   # Algae excluding giant kelp
  
# SESSILE INVERTEBRATES
sbc.sessile.temp <- comm.dat.long %>%
  dplyr::filter(TAXON_GROUP == "SESSILE INVERT") %>%
  dplyr::select(-TAXON_GROUP)

sbc.sessile <- rbind(sbc.xy, sbc.env, sbc.sessile.temp); rm(sbc.sessile.temp)

# MOBILE INVERTEBRATES
sbc.mobile.temp <- comm.dat.long %>%
  dplyr::filter(TAXON_GROUP == "MOBILE INVERT") %>%
  dplyr::select(-TAXON_GROUP)

sbc.mobile <- rbind(sbc.xy, sbc.env, sbc.mobile.temp); rm(sbc.mobile.temp)

# FISHES
sbc.fish.temp <- comm.dat.long %>%
  dplyr::filter(TAXON_GROUP == "FISH") %>%
  dplyr::select(-TAXON_GROUP)

sbc.fish <- rbind(sbc.xy, sbc.env, sbc.fish.temp); rm(sbc.fish.temp)


# ---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------


# ===================================================================================================
# SELECT ONLY THE DATASET OF INTEREST FOR THIS SCRIPT
# ===================================================================================================
#dat.long <- sbc.algae.no.kelp
dat.long <- sbc.algae.w.kelp
#dat.long <- sbc.sessile
#dat.long <- sbc.mobile
#dat.long <- sbc.fish

# ---------------------------------------------------------------------------------------------------
# MAKE DATA LIST
dat <- list()

# COMMUNITY DATA 
comm.long <- dat.long[dat.long$OBSERVATION_TYPE == "TAXON_COUNT", ] 
comm.long <- comm.long %>%
  droplevels()

str(comm.long)  # Inspect the structure of the community data

#Add number of unique taxa and number of years to data list:
dat$n.spp <- length(levels(comm.long$VARIABLE_NAME))
dat$n.years <- length(unique(comm.long$DATE))
# Ensure that community data VALUE and DATE are coded as numeric
comm.long <- comm.long %>%   # Recode if necessary
  mutate_at(vars(c(DATE, VALUE)), as.numeric)

# Ensure that community character columns coded as factors are re-coded as characters
comm.long <- comm.long %>%   # Recode if necessary
  mutate_if(is.factor, as.character)
  
# Ensure that SITE_ID is a character: recode numeric as character 
comm.long <- comm.long %>%   # Recode if necessary
  mutate_at(vars(SITE_ID), as.character)

# Double-check that all columns are coded properly
ifelse(FALSE %in% 
   c(
     class(comm.long$OBSERVATION_TYPE) == "character",
     class(comm.long$SITE_ID) == "character",
     class(comm.long$DATE) == "numeric",
     class(comm.long$VARIABLE_NAME) == "character",
     class(comm.long$VARIABLE_UNITS) == "character",
     class(comm.long$VALUE) == "numeric"
     #class(comm.long$TAXON_GROUP) == "character")
   ),
  "ERROR: Community columns incorrectly coded.", 
  "OK: Community columns correctly coded.")

# ---------------------------------------------------------------------------------------------------
# Check balanced sampling of species across space and time by inspecting table, and add to data list
xtabs(~ SITE_ID + DATE, data = comm.long)
hist(na.omit(comm.long$DATE))

ifelse(length(unique(xtabs(~ SITE_ID + DATE, data = comm.long))) == 1,
       "OK: Equal number of taxa recorded across space and time.", 
       "ERROR: Unequal numbers of observations across space and time, or taxa list not fully propagated across space and time. Inspect contingency table.")

# ---------------------------------------------------------------------------------------------------
# Add to dat list the unique taxa
dat$comm.long <- comm.long

# Convert community data to wide form
comm.wide <- comm.long %>%
  dplyr::select(-VARIABLE_UNITS) %>%
  spread(key = VARIABLE_NAME,  value = VALUE)

dat$comm.wide <- comm.wide
summary(dat)

# ---------------------------------------------------------------------------------------------------
# SPATIAL DATA
# Check for and install required packages

for (package in c('dplyr', 'tidyr', 'XML', 'sp', 'geosphere', 'rgdal','maps','reshape2','ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#pull out coordinate data and make sure that it is numeric
cord <- filter(dat.long, OBSERVATION_TYPE=="SPATIAL_COORDINATE");cord
cord$SITE_ID <- toupper(cord$SITE_ID)  # Ensure sites are in all caps
cord <- droplevels(cord)
str(cord)

cord.wide <- cord %>%
  select(-VARIABLE_UNITS) %>%
  spread(VARIABLE_NAME,  VALUE)

cord.wide

sites <- c(unique(cord.wide$SITE_ID));sites

cord.wide$latitude <- as.numeric(as.character(cord.wide$LAT)) 
cord.wide$longitude <- as.numeric(as.character(cord.wide$LONG)) #cord.wide$longitude <- as.numeric(as.character(cord.wide$longitude))
cord.wide <- cord.wide[c("longitude", "latitude")] #pull last two columns and reorder

#add number of sites and long/lat coords to data list:
dat$n.sites <- length(sites)
dat$longlat <- cord.wide

#make data spatially explicit
coordinates(cord.wide) = c("longitude", "latitude")
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(cord.wide) <- crs.geo  # define projection system of our data to WGS84 

summary(cord.wide) 

#create a distance matrix between sites, best fit distance function TBD
distance.mat <- (distm(cord.wide, fun = distVincentyEllipsoid)/1000);distance.mat #km distance
rownames(distance.mat) <- sites
colnames(distance.mat) <- sites

#add distance matrix to data list and maximum distance between sites (km) to data list
dat$distance.mat <- distance.mat
dat$max.distance <- max(distance.mat)
summary(dat)

# ---------------------------------------------------------------------------------------------------
# ENVIRONMENTAL COVARIATES
env.long <- subset(dat.long, OBSERVATION_TYPE == "ENV_VAR")
env.long <- droplevels(env.long)
str(env.long)

# Convert from long to wide
env.wide <- env.long %>%
  select(-VARIABLE_UNITS) %>%
  tidyr::spread(VARIABLE_NAME,  VALUE)

# Add environmental covaiates to data list

dat$n.covariates <- length(levels(env.long$VARIABLE_NAME))
dat$cov.names <- levels(env.long$VARIABLE_NAME)
dat$env <- env.wide
dat$env.long <- env.long

#CHECK: 
nrow(dat$comm.wide); nrow(dat$env); dat$n.years * dat$n.sites
# Are all year-by-site combinations in community data matched by environmental data?
ifelse(nrow(dat$comm.wide) == nrow(dat$env), "Yes", "No")

# Are community data balanced over space and time?
ifelse(nrow(dat$comm.wide) == dat$n.years * dat$n.sites, "Yes", "No")

# Inspect summary of 'dat' list 
summary(dat)

#clean up the workspace
rm("comm.long","comm.wide","cord","cord.wide","crs.geo","dat.long", "data.key", "data.set","distance.mat","env.long", "env.wide","package","sites")
ls()

# Write CSV file for cleaned data (L3)
write.csv(out_long, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-sbc-algae-castorani.csv", row.names = F)

