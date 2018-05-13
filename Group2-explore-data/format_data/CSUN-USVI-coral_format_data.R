### Cleaning Cal State Northridge (CSUN) U.S. Virgin Islands coral data

# --------------------------------------------------------------------------------------------------------------------------------

# Max Castorani
# 8 June 2017

## Data manipulation packages
library(dplyr)
library(tidyr)

source("Group2-explore-data/format_data/pull_data_gdrive_fun.R")

# --------------------------------------------------------------------------------------------------------------------------------
#These data are in the ARCHIVE at "~/Google Drive/LTER Metacommunities/LTER_DATA/ARCHIVE/CSUN-USVI-coral-Castorani/CSUN_USVI_taxa_at_random_sites_long_20160106.csv"
## Read in the data
csun.usvi.coral <- read_csv_gdrive("0BxUZSA1Gn1HZMmk0NDNiTXdwTDQ") %>%
  tbl_df()

#Google Drive File Stream: (copied USVI data from ARCHIVE folder to L0 folder)
csun.usvi.coral <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/CSUN-USVI-coral-Castorani/CSUN_USVI_taxa_at_random_sites_long_20160106.csv", stringsAsFactors = FALSE) %>%
  tbl_df()


# Explore data
unique(csun.usvi.coral$taxa)

# Replace underscores with dots for convenience. Also convert to lowercase.
colnames(csun.usvi.coral) <- tolower(gsub("_", ".", colnames(csun.usvi.coral)))

csun.usvi.coral_clean <- csun.usvi.coral %>%
  dplyr::select(-image.id) %>%
  dplyr::mutate(uniqueID = paste(site, quadrat, year, sep = ".")) %>%
  dplyr::mutate(temp = paste0(uniqueID, ".", taxa),
                percent.cover = as.numeric(as.character(percent.cover))) %>%
  group_by(temp) %>%   # Fix duplicate observations by averaging
  dplyr::summarise(site = unique(site),
                   quadrat = unique(quadrat),
                   year = unique(year),
                   taxa = unique(taxa),
                   percent.cover = mean(percent.cover, na.rm = TRUE)) %>%
  dplyr::select(-temp) 

rownames(csun.usvi.coral_clean) <- NULL

# Convert from long to wide and back to long to be sure that we have fully propagated taxa
csun.usvi.coral_clean_wide <- spread(csun.usvi.coral_clean, key = taxa, value = percent.cover, fill = 0)  # Note: NA replaced with 0
csun.usvi.coral_clean_long <- gather(csun.usvi.coral_clean_wide, key = taxa, value = percent.cover, -year, -quadrat, -site)

# Finish cleaning data by renaming and adding columns
csun.usvi.coral_clean <- csun.usvi.coral_clean_long %>%
  dplyr::rename(SITE_ID = site,
                DATE = year,
                SUB_SITE_ID = quadrat,
                VALUE = percent.cover,
                VARIABLE_NAME = taxa) %>%  
  dplyr::mutate(OBSERVATION_TYPE = "TAXON_COUNT",
                VARIABLE_UNITS = "percent.cover",
                UNIQUE_SPATIAL_ID = paste(SITE_ID, SUB_SITE_ID, sep = "_")) %>%
  dplyr::select(OBSERVATION_TYPE,
                SITE_ID, 
                SUB_SITE_ID, 
                UNIQUE_SPATIAL_ID,
                DATE,
                VARIABLE_NAME,
                VARIABLE_UNITS,
                VALUE)

# Remove unneeded files
rm(csun.usvi.coral, csun.usvi.coral_clean_long, csun.usvi.coral_clean_wide)

# --------------------------------------------------------------------------------------------------------------------------------

# Write CSV file for cleaned data (L2. Skipping L1 because data are already aggregated by year)
write.csv(csun.usvi.coral_clean, file = "L2-csun.usvi-coral-castorani.csv")

# --------------------------------------------------------------------------------------------------------------------------------
# Aggregate by site, then add spatial information
csun.usvi.coral_L3 <- csun.usvi.coral_clean %>%
  dplyr::group_by(SITE_ID, DATE, VARIABLE_NAME) %>%
  dplyr::summarise(OBSERVATION_TYPE = unique(OBSERVATION_TYPE),
                   VARIABLE_UNITS = unique(VARIABLE_UNITS),
                   VALUE = mean(VALUE, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE) 
  as.data.frame(.)

spatial.coords <- data.frame(
  "OBSERVATION_TYPE" = rep("SPATIAL_COORDINATE", length(unique(csun.usvi.coral_L3$SITE_ID))*2),
  "SITE_ID" = rep(unique(csun.usvi.coral_L3$SITE_ID), times = 2),
  "DATE" = rep("NA", length(unique(csun.usvi.coral_L3$SITE_ID))*2),
  "VARIABLE_NAME" = c(rep("LAT", length(unique(csun.usvi.coral_L3$SITE_ID))),
                      rep("LONG", length(unique(csun.usvi.coral_L3$SITE_ID)))
                      ),
  "VARIABLE_UNITS" = rep("dec. degrees", length(unique(csun.usvi.coral_L3$SITE_ID))*2),
  "VALUE" = c(18.30713526,
              18.31095579,
              18.31673677,
              18.31725847,
              18.3125466,
              18.31445176,
              -64.72164419,
              -64.72181924,
              -64.72998517,
              -64.72779501,
              -64.72301093,
              -64.73130735)
 ) %>%
  dplyr::mutate(OBSERVATION_TYPE = as.character(OBSERVATION_TYPE),
                DATE = as.numeric(DATE),
                VARIABLE_NAME = as.character(VARIABLE_NAME),
                VARIABLE_UNITS = as.character(VARIABLE_UNITS)) %>%
  dplyr::mutate(DATE = NA)


csun.usvi.coral_L3_final <- rbind(spatial.coords, csun.usvi.coral_L3)


# Write CSV file for cleaned data (L3)
write.csv(csun.usvi.coral_L3_final, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-usvi-coral-castorani.csv", row.names = F)
