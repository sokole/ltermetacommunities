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

## Read in the data
mcr.fish <- read_csv_gdrive("0BxUZSA1Gn1HZRUVMenVlUndsRnM") %>%
  tbl_df()

#Google Drive File Stream method:
mcr.fish <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/MCR-fish/MCR_LTER_Annual_Fish_Survey_20160509.csv", stringsAsFactors = FALSE)

# Replace underscores with dots for convenience. Also convert to lowercase.
colnames(mcr.fish) <- tolower(gsub("_", ".", colnames(mcr.fish)))

# Code species guild
mcr.fish <- mcr.fish %>%
  dplyr::rename(species = taxonomy) %>%
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
write.csv(mcr.fish_reformat, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L2-mcr-fish-castorani.csv", row.names = F)


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
write.csv(mcr.fish_L3_final, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-mcr-fish-castorani.csv", row.names = F)