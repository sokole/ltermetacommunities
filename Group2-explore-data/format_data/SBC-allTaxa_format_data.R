# --------------------------------------------------------- #
# Format raw data as a list of tables                       #
# separate by taxa to make four separate L3 datasets                                  #
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
#plot coordinates and wave height data will be merged in from this file.

# Assign data set of interest
# NOTE: Google Drive file ID is different for each dataset

# SBC LTER (Santa Barbara Coastal): Macroalgae
data.set <- "SBC-algae"
data.key <- "0BxUZSA1Gn1HZRUxaNmV1Y21abmc" # Google Drive file ID for 'LTER-DATA/L0-raw/SBC/sbc_algae_dat_long.csv'

dat.long.old <-  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", data.key), stringsAsFactors=F) %>%
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
                 "GROWTH_MORPH"    ), check.names=TRUE, stringsAsFactors=F)


# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DATE) == length(tmp1DATE[!is.na(tmp1DATE)])){dt1$DATE <- tmp1DATE } else {print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DATE)


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
                 "site"    ), check.names=TRUE, stringsAsFactors=F)


# attempting to convert dt1$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1Date<-as.Date(dt1$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1Date) == length(tmp1Date[!is.na(tmp1Date)])){dt1$Date <- tmp1Date } else {print("Date conversion failed for dt1$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1Date) 

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



# Write CSV file for each cleaned (L3) dataset
write.csv(sbc.algae.w.kelp, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-sbc-algae-castorani.csv", row.names = F)

write.csv(sbc.sessile, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-sbc-sessileInverts-castorani.csv", row.names = F)

write.csv(sbc.mobile, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-sbc-mobileInverts-castorani.csv", row.names = F)

write.csv(sbc.fish, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-sbc-fish-castorani.csv", row.names = F)
