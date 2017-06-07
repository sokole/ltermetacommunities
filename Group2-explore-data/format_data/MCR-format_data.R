### Cleaning MCR Algae Data, MCR Coral Data, and compile into one dataset for the site

# --------------------------------------------------------------------------------------------------------------------------------

# Max Castorani
# 7 June 2017

### MCR Algae Data ###

## Data manipulation packages
library(dplyr)
library(tidyr)

source("Group2-explore-data/format_data/MCR-data_import_fun.R")

## Read in the data
mcr.algae <- read_csv_gdrive("insert google drive id here") %>%
  tbl_df()

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

### MCR Coral Data ###

## Read in the data
mcr.coral <- read_csv_gdrive("insert google drive id here") %>%
  tbl_df()

# Replace underscores with dots for convenience. Also convert to lowercase.
colnames(mcr.coral) <- tolower(gsub("_", ".", colnames(mcr.coral)))

# Code species guild
mcr.coral <- mcr.coral %>%
  dplyr::rename(species = taxonomy...substrate...functional.group) %>%
  dplyr::filter(species != "Sand",              # Drop species that are non-coral
                species != "Unknown or Other",
                species != "Macroalgae",
                species != "Crustose Coralline Algae / Bare Space",
                species != "Turf",
                species != "Non-coralline Crustose Algae",
                !is.na(species),
                !is.na(percent.cover)) %>%
  droplevels() %>%
  # Convert date to year
  mutate(year = as.numeric(strtrim(as.character(date), 4)))

# For each species, average the abundance data by year, habitat, plot ('site'), and subplot ('transect')
mcr.coral_clean <- mcr.coral %>%
  group_by(year, site, habitat, transect, species) %>%
  dplyr::summarise(percent.cover = mean(percent.cover, na.rm = TRUE)) %>%
  ungroup() %>%
  droplevels() 

# Convert from long to wide and back to long to be sure that we have fully propagated taxa
mcr.coral_clean_wide <- spread(mcr.coral_clean, key = species, value = percent.cover, fill = 0)
mcr.coral_clean_long <- gather(mcr.coral_clean_wide, key = species, value = percent.cover, -year, -site, -transect, -habitat)

# Finish cleaning data by renaming and adding columns
mcr.coral_clean <- mcr.coral_clean_long %>%
  mutate(project = "coral",  # rename what they called site to what we call project
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
         guild = "coral") %>%
  select(year, site, habitat, project, plot, subplot, uniqueID, guild, species, abundance, unitAbund, scaleAbund) #, growth)

# Remove unneeded files
rm(mcr.coral, mcr.coral_clean_long, mcr.coral_clean_wide)

# --------------------------------------------------------------------------------------------------------------------------------

### MCR Invertebrate Data ###

## Read in the data
mcr.inverts <- read_csv_gdrive("insert google drive id here") %>%
  tbl_df()

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

### MCR Fish Data ###

## Read in the data
mcr.fish <- read_csv_gdrive("insert google drive id here") %>%
  tbl_df()

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

### Combined datasets for all taxa

mcr <- rbind(mcr.algae_clean, mcr.coral_clean, mcr.inverts_clean, mcr.fish_clean)

rm(mcr.coral_clean, mcr.algae_clean, mcr.inverts_clean, mcr.fish_clean)

