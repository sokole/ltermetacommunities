### Cleaning MOOREA CORAL REEF (MCR) data

# --------------------------------------------------------------------------------------------------------------------------------

# Max Castorani
# 8 June 2017

## Data manipulation packages
library(dplyr)
library(tidyr)

source("Group2-explore-data/format_data/pull_data_gdrive_fun.R")

# --------------------------------------------------------------------------------------------------------------------------------

### MCR Algae Data ###

## Read in the data
mcr.algae <- read_csv_gdrive("0BxUZSA1Gn1HZRGRYQXVIckdKQjA") %>%
  tbl_df()

#local path:
mcr.algae <- read.csv("~/Google Drive/LTER Metacommunities/LTER-DATA/L0-raw/MCR-algae/MCR_LTER_Annual_Survey_Benthic_Cover_20151023.csv", stringsAsFactors = FALSE)



# Replace underscores with dots for convenience. Also convert to lowercase.
colnames(mcr.algae) <- tolower(gsub("_", ".", colnames(mcr.algae)))

# Fix species 
mcr.algae <- mcr.algae %>%
  dplyr::rename(species = taxonomy.substrate.functional.group) %>%
  dplyr::filter(species != "Coral",  # Remove the non-focal taxa (non-algae)
                species != "Ascidians",
                species != "Sponge",
                species != "Shell Debris",
                species != "Coral Rubble",
                species != "No data",
                species != "Bare Space",
                species != "Soft Coral",
                species != "Sand",
                species != "",
                !is.na(species)
  ) %>%
  droplevels()

# Clean the dataset
mcr.algae$year <- as.numeric(as.character(mcr.algae$year))
mcr.algae$year[is.na(mcr.algae$year)] <- 2006

# Aggregate data by year, taxon, habitat, transect and site (6 sites total)
mcr.algae_clean <- mcr.algae %>%
  group_by(year, site, habitat, transect, species) %>%
  dplyr::summarise(percent.cover = mean(percent.cover, na.rm = TRUE)) %>%
  ungroup() %>%
  droplevels() 

# Convert from long to wide and back to long to be sure that we have fully propagated taxa
mcr.algae_clean_wide <- spread(mcr.algae_clean, key = species, value = percent.cover, fill = 0)
mcr.algae_clean_long <- gather(mcr.algae_clean_wide, key = species, value = percent.cover, -year, -site, -transect, -habitat)

# Finish cleaning data by renaming and adding columns
mcr.algae_clean <- mcr.algae_clean_long %>%
  mutate(project = "algae",  # rename what they called site to what we call project
         plot = site) %>%
  select(-site) %>%
  mutate(site = "mcr",   # format column names
         plot = paste0("location_", sapply(strsplit(as.character(plot), " "), tail, 1)),
         subplot = paste0("transect_", transect),
         abundance = percent.cover,  
         unitAbund = "mean.percent.cover",
         scaleAbund = "0.25_m2",
         growth = NA,
         uniqueID = paste(site, project, plot, subplot, sep = "_"),
         guild = "algae") %>%
  select(year, site, habitat, project, plot, subplot, uniqueID, guild, species, abundance, unitAbund, scaleAbund) #, growth)

# Remove unneeded files
rm(mcr.algae, mcr.algae_clean_long, mcr.algae_clean_wide)

# --------------------------------------------------------------------------------------------------------------------------------

# Reformat column names
mcr.algae_reformat <- mcr.algae_clean %>%
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
write.csv(mcr.algae_reformat, file = "L2-mcr-algae-castorani.csv", row.names = F)

# --------------------------------------------------------------------------------------------------------------------------------
# Aggregate by site, then add spatial information
mcr.algae_L3 <- mcr.algae_reformat %>%
  dplyr::group_by(SITE_ID, DATE, VARIABLE_NAME) %>%
  dplyr::summarise(OBSERVATION_TYPE = unique(OBSERVATION_TYPE),
                   VARIABLE_UNITS = unique(VARIABLE_UNITS),
                   VALUE = mean(VALUE, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE) %>%
  as.data.frame(.)

# Replace underscores with dots in location IDs for future plotting. 
mcr.algae_L3$SITE_ID <- gsub("_", "", mcr.algae_L3$SITE_ID)

spatial.coords <- data.frame(
  "OBSERVATION_TYPE" = rep("SPATIAL_COORDINATE", length(unique(mcr.algae_L3$SITE_ID))*2),
  "SITE_ID" = rep(unique(mcr.algae_L3$SITE_ID), times = 2),
  "DATE" = rep("NA", length(unique(mcr.algae_L3$SITE_ID))*2),
  "VARIABLE_NAME" = c(rep("LAT", length(unique(mcr.algae_L3$SITE_ID))),
                      rep("LONG", length(unique(mcr.algae_L3$SITE_ID)))
                      ),
  "VARIABLE_UNITS" = rep("dec. degrees", length(unique(mcr.algae_L3$SITE_ID))*2),
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

mcr.algae_L3_final <- rbind(spatial.coords, mcr.algae_L3)

# Write CSV file for cleaned data (L3)
write.csv(mcr.algae_L3_final, file = "~/Google Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-mcr-algae-castorani.csv", row.names = F)
