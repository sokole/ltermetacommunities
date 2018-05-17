### Cleaning MOOREA CORAL REEF (MCR) data

# --------------------------------------------------------------------------------------------------------------------------------

# Max Castorani
# 8 June 2017

## Data manipulation packages
library(dplyr)
library(tidyr)

source("Group2-explore-data/format_data/pull_data_gdrive_fun.R")



### MCR Invertebrate Data ###

## Read in the data
mcr.inverts <- read_csv_gdrive("0BxUZSA1Gn1HZU2hQdC0wVVNQdDA") %>%
  tbl_df()

#Google Drive File Stream method:
mcr.inverts <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/MCR-inverts/MCR_LTER_Annual_Survey_Herbiv_Invert_20150330.csv", stringsAsFactors = FALSE)



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
write.csv(mcr.inverts_reformat, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L2-mcr-inverts-castorani.csv", row.names = F)


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
write.csv(mcr.inverts_L3_final, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-mcr-inverts-castorani.csv", row.names = F)
