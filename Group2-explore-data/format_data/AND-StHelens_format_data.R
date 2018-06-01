
library(tidyr)
library(dplyr)


# your working directory
source("Group1-finding-data/util.R")

# read data online
spp_abundance <- read.csv('http://esapubs.org/archive/ecol/E091/152/MSH_SPECIES_PLOT_YEAR.csv', stringsAsFactors=F) %>%
                   gather(species, cover, Abilas:Xerten) %>%
                   setNames( tolower(names(.)) )
plot_d <- read.csv('http://esapubs.org/archive/ecol/E091/152/MSH_PLOT_DESCRIPTORS.csv', fileEncoding = "ISO-8859-1") %>%
                   setNames( tolower(names(.)) )
taxa_d  <- read.csv('http://esapubs.org/archive/ecol/E091/152/MSH_SPECIES_DESCRIPTORS.csv', fileEncoding = "ISO-8859-1")

# #ALTERNATIVE: read in from version cached on Google Drive
 spp_abundance <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/AND-plants-mtStHelen/MSH_SPECIES_PLOT_YEAR.csv", stringsAsFactors=F) %>% 
                     gather(species, cover, Abilas:Xerten) %>% 
                     setNames( tolower(names(.)) )
# 
 plot_d <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/AND-plants-mtStHelen/MSH_PLOT_DESCRIPTORS.csv",fileEncoding = "ISO-8859-1", stringsAsFactors=F) %>% 
                     setNames( tolower(names(.)) )



# Format data ----------------------------------------------------------------------

# Remove non-existing data from plot table
plot_info <- subset(plot_d ,plot_code !="")

# Create "plot code" in spp_abundance data frame
plot_num  <- spp_abundance$plot_number
plot_n_ch <- as.character(plot_num)
plot_n_ch[which(plot_num < 10)] <- paste0("0",
                                   plot_n_ch[which(plot_num < 10)])
plot_code_vec <- paste0(spp_abundance$plot_name, plot_n_ch)
spp_abundance <- mutate(spp_abundance, plot_code = plot_code_vec)


# Prepare three "observation type" tables 

# Spatial coordinates --------------------------------------------------
plot_long   <- format_data(plot_info,
                           OBSERVATION_TYPE = "ENV_VAR", 
                           SITE_ID = "plot_code", 
                           DATE = NA, # NA because non-temporal
                           VARIABLE_NAME = "longitude", 
                           VARIABLE_UNITS = "dec.degrees",  
                           VALUE = "long...")

plot_lat   <- format_data(plot_info,
                           OBSERVATION_TYPE = "ENV_VAR", 
                           SITE_ID = "plot_code", 
                           DATE = NA, # NA because non-temporal
                           VARIABLE_NAME = "latitude", 
                           VARIABLE_UNITS = "dec.degrees",  
                           VALUE = "lat...")

# Stack longitude and latitude together
plot_coord  <- rbind(plot_long,plot_lat)


# Species abundance data -------------------------------------------
spp_abundance <- rename(spp_abundance,VALUE = cover,
                        SITE_ID = plot_code,
                        VARIABLE_NAME = species,
                        DATE = year)
spp_abundance <- mutate(spp_abundance, 
                        OBSERVATION_TYPE = "TAXON_COUNT",
                        VARIABLE_UNITS = "PERCENT_COVER")
spp_abundance <- order_col(spp_abundance)


# Environmental data -----------------------------------------------

# heat load (could not find unit of measure. 
# Index is from McCune and Keon 2002 in JVS)
heat_load   <- format_data(plot_info,
                            OBSERVATION_TYPE = "ENV_VAR", 
                            SITE_ID = "plot_code", 
                            DATE = NA, # NA because non-temporal
                            VARIABLE_NAME = "heat_load", 
                            VARIABLE_UNITS = "Adimensional(0 to 1)",  
                            VALUE = "heat_load")

# Elevation
elevation_m   <- format_data(plot_info,
                            OBSERVATION_TYPE = "ENV_VAR", 
                            SITE_ID = "plot_code", 
                            DATE = NA, # NA because non-temporal
                            VARIABLE_NAME = "elevation", 
                            VARIABLE_UNITS = "meter",  
                            VALUE = "elevation.m.")

# Aspect (according to Arabic compass rose)
aspect   <- format_data(plot_info,
                             OBSERVATION_TYPE = "ENV_VAR", 
                             SITE_ID = "plot_code", 
                             DATE = NA, # NA because non-temporal
                             VARIABLE_NAME = "aspect", 
                             VARIABLE_UNITS = "Arabic_compass_rose",  
                             VALUE = "aspect")

# Slope in decimal degrees
slope   <- format_data(plot_info,
                        OBSERVATION_TYPE = "ENV_VAR", 
                        SITE_ID = "plot_code", 
                        DATE = NA, # NA because non-temporal
                        VARIABLE_NAME = "slope", 
                        VARIABLE_UNITS = "dec.degrees",  
                        VALUE = "slope...")

# Succession type (e.g. primary if all plant life removed,
# secondary if a few survivors were initially present
# disturbance in places where most species survived)
succession <-  format_data(plot_info,
                           OBSERVATION_TYPE = "ENV_VAR", 
                           SITE_ID = "plot_code", 
                           DATE = NA, # NA because non-temporal
                           VARIABLE_NAME = "succession_type", 
                           VARIABLE_UNITS = "categorical",  
                           VALUE = "succession_type")

# Type of disturbance
impact_type <-  format_data(plot_info,
                           OBSERVATION_TYPE = "ENV_VAR", 
                           SITE_ID = "plot_code", 
                           DATE = NA, # NA because non-temporal
                           VARIABLE_NAME = "impact_type", 
                           VARIABLE_UNITS = "categorical",  
                           VALUE = "impact_type")

# Stack these four data types on top of each other
env_vars <- rbind(heat_load, elevation_m, aspect, slope, 
                  succession, impact_type)

# Output file =========================================================
delmoral_data <- rbind(plot_coord,spp_abundance,env_vars) %>% 
                    # select Abraham Plain sites located on PUMICE. 
                    # This is the longest swath of continuous data
                    subset( grepl('PUPL', SITE_ID) )

write.csv(delmoral_data, "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-and-plants-mtStHelens.csv", row.names = F)
