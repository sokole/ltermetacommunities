##--------------------------------------------------------------------------------
## Author: Christopher P. Catano                                               
## Creation: February 1, 2019                                                    
##                                                                             
## Discription: Combine metacommunity response and predictor variables into a 
##              single data set to be used in analysis.
##              
##--------------------------------------------------------------------------------


rm(list = ls())

# Check for and install required packages
for (package in c('tidyverse', 'PerformanceAnalytics', 'ggthemes', 
                  'gridExtra', 'grid', 'viridis')) {
  if (!require(package, character.only = T, quietly = T)) {
    install.packages(package)
    library(package, character.only = T)
  }
}


# 1. IMPORT DATA SETS ------------------------------------------------------------

# Metacommunity characteristics and site data
metacom.data <- read.csv(here("Manuscripts/MS3/data/L3_DATA_list.csv"))
# Change name of "LTEr.name" to match environmantal data
colnames(metacom.data)[5] <- "site"

# Environmental data
env.data <- read.csv(here("Manuscripts/MS3/data/lter_centroid_satdata.csv"))

# Change site name for USVI so matching in both data sets
env.data$site <- as.character(env.data$site)
env.data <- env.data %>%
  mutate(site = replace(site, site == "CSUN-USVI", "USVI")) 
env.data$site <- as.factor(env.data$site)

# Check data
str(metacom.data)

# Drop unnecessasry columns and remove sites that were "REJECTED" or have no data
# Dispersal.habit and mobility map 1:1, keep dispersal.habit
metacom.data <- metacom.data %>%
  filter(!(L3.status == 'REJECTED')) %>%
  select(-c(google_id, L0_data_directory, contact, L3.status, 
            responsible.person, provided.identifier, doi, date.accessed, 
            date.published, source_url_or_contact, mobility)) 

summary(metacom.data$biome)
# brackish  freshwater  marine  terrestrial 
#   2         3          9         14 
# not enough replication in brackish or freshwater to be useful, combine with 
# marine (i.e., aquatic) or combine with terrestrial? Which is more relevant?

summary(metacom.data$body.size)
# macro  meso micro 
# 20     6     2 
# only 2 in micro, two small to be its own group? Maybe make 2 (large, small?)

summary(metacom.data$dispersal.habit)
#  active passive 
#   17      11 

summary(metacom.data$trophic.group)
# consumers      primary producers 
#   19                 9 

summary(metacom.data$organism_group)
# algae  birds  coral  fish  herps  invertebrates 
#  2       3     2      5      2          6

# macroalgae  plants  sessile invertebrates  zooplankton
#     1         5            1                   1

# Not enough replication is most (all?) groups, likely not a useful factor

# NOTES
# All "mobile" dispersers are also "consumers", all but two "sessile" dispersers
# are "producers", thus both columns (dispersal.habit and trophic.group) can't be 
# used in the model. Dispersal.habit is the broader, and more fundamental category 
# relating to metacommunity features/dyanmics

# Seems like body size, biome, and dispersal.habit are the most relevant/orthogonal
# factors to predict metacommunity stability characteristics. The others may be
# useful to contextualize or highlight in the data, but not statistically asses.

# n.years and n.years are likely important covariates that can influence stability metrics
# and should be considered. shorter time series are expected to be noiser (temporally) 
# and metacommunities with fewer sites are likely to be more spatially asynchronous


# Keep sites in env.data that are in metacom.data (this is automatically done in left_join)
keep.env <- env.data[env.data$site %in% metacom.data$site, ]
# Check that all sites in metacom.data are found in keep.env (should be 0)
length(row.names(metacom.data[!(metacom.data$site %in% keep.env$site), ]))
keep.env <- keep.env %>%
  select(-radius)

#site with multiple rows:
# Keep SBC_mainland_coast_centroid
SBC <- keep.env %>%
  filter(subsite == "SBC_mainland_coast_centroid") 
  
NTL <- keep.env %>%
  filter(site == "NTL" & subsite != "northern_lakes_no_bogs_centroid" & 
           subsite != "northern_lakes_with_bogs_centroid") %>%
  group_by(site) %>% 
  summarize(lat = mean(lat),
            lon = mean(lon),
            chla_temporal_sd = mean(chla_temporal_sd),
            lst_temporal_sd = mean(lst_temporal_sd),
            ndvi_temporal_sd = mean(ndvi_temporal_sd),
            sst_temporal_sd = mean(sst_temporal_sd),
            bathymetry_spatial_sd_slope_60km = mean(bathymetry_spatial_sd_slope_60km),
            elevation_spatial_sd_slope_60km = mean(elevation_spatial_sd_slope_60km))
NTL <- data.frame(append(NTL, list(subsite = NA), after=match("site", names(NTL))))


USVI <- keep.env %>%
  filter(site == "USVI") %>%
  group_by(site) %>% 
  summarize(lat = mean(lat),
            lon = mean(lon),
            chla_temporal_sd = mean(chla_temporal_sd),
            lst_temporal_sd = mean(lst_temporal_sd),
            ndvi_temporal_sd = mean(ndvi_temporal_sd),
            sst_temporal_sd = mean(sst_temporal_sd),
            bathymetry_spatial_sd_slope_60km = mean(bathymetry_spatial_sd_slope_60km),
            elevation_spatial_sd_slope_60km = mean(elevation_spatial_sd_slope_60km))
USVI <- data.frame(append(USVI, list(subsite = NA), after=match("site", names(USVI))))

JRN <- keep.env %>%
  filter(site == "JRN") %>%
  group_by(site) %>% 
  summarize(lat = mean(lat),
            lon = mean(lon),
            chla_temporal_sd = mean(chla_temporal_sd),
            lst_temporal_sd = mean(lst_temporal_sd),
            ndvi_temporal_sd = mean(ndvi_temporal_sd),
            sst_temporal_sd = mean(sst_temporal_sd),
            bathymetry_spatial_sd_slope_60km = mean(bathymetry_spatial_sd_slope_60km),
            elevation_spatial_sd_slope_60km = mean(elevation_spatial_sd_slope_60km))
JRN <- data.frame(append(JRN, list(subsite = NA), after=match("site", names(JRN))))

keep.env2 <- keep.env %>%
  filter(site != "JRN" & site != "USVI" & site != "NTL" & site != "SBC")

# Combine aggregated site data to craete single data frame with env. variables
env <- rbind(keep.env2, JRN, NTL, SBC, USVI)
            
# Create column to combine sites by (to make sure "AND" sites match correctly)
metacom.data$match <- metacom.data$site
metacom.data$match <- as.character(metacom.data$match)
metacom.data[metacom.data$dataset_id == "and-plants-mtStHelens", ]$match <- "MtH"
            
env$match <- env$site
env$match <- as.character(env$match)
env[1, ]$match <- "MtH"
env <- env %>%
  select(-site)


# Combine environmental and metacommunity data into a single data frame.
comb.data <- metacom.data %>%
  left_join(env, by = "match") %>%
  select(-c(match, subsite, chla_temporal_sd))


# Create column for temporal variation in temp, and spatial env heterogeneiety.
# temp: marine sites use "sst..."; freshwater and terrestrail use "lst..."
comb.data$temp_temporal_sd <- comb.data$lst_temporal_sd
comb.data[comb.data$biome == "marine", ]$temp_temporal_sd <- comb.data[comb.data$biome == "marine", ]$sst_temporal_sd

# env: marine sites use "bathymetry..."; freshwater and terrestrial use "elevation..."
comb.data$env_heterogeneity <- comb.data$elevation_spatial_sd_slope_60km
comb.data[comb.data$biome == "marine", ]$env_heterogeneity <- comb.data[comb.data$biome == "marine", ]$bathymetry_spatial_sd_slope_60km

comb.data <- comb.data %>%
  select(-c(lst_temporal_sd, sst_temporal_sd, elevation_spatial_sd_slope_60km, bathymetry_spatial_sd_slope_60km))


# Read in the metacommunity variability responses sent by Eric Sokal
response <- read.csv(here("Manuscripts/MS3/data/L4_metacommunity_variability_analysis_results_2022-02-10.csv"))

# Check conrguence between data sets in this file with data available from the
# environmental file

# Data sets with no metacommunity data
missing <- response[!response$dataset_file_name %in% comb.data$l3_filename, ]
unique(missing$dataset_file_name)
#[1] L3-gce-mollusc-compagnoni.csv    L3-sev-plants-compagnoni.csv    
#[3] L3-sev-arthropods-compagnoni.csv L3-knz-fish-compagnoni.csv 

# Combine and export data set for metaanalysis
colnames(response)[1] <- "l3_filename"
all.data <- comb.data %>%
  left_join(response, by = "l3_filename")

length(unique(all.data$l3_filename))
#28 data sets

write.csv(all.data, here("Manuscripts/MS3/output/metacom_data_for_models.csv"))
