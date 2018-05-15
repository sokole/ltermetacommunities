# Aldo Compagnoni: script to retrieve time series data using
# the ecoretriever package. 
# Found 9 time series data sets.
library(devtools)
install_github("ropensci/ecoretriever")

library(devtools)
install_github('ropensci/ecoretriever')
rdataretriever::datasets()

# Attach the package and the database
library("rdataretriever") ; library(dplyr)
library("RSQLite") 

# your working directory
dir <- "C:/CODE/ltermetacommunities/Group1-finding-data/"
setwd(dir)
source(paste0(dir,"util.R"))
# List the datasets available via the Retriever
ecoRetDataList <- rdataretriever::datasets()

rdataretriever::download('DelMoral2010')

# Found a few temporal data sets.
temporalData1 <- c( "Adler2007","Zachmann2010","Steppe_plants_2013",
                    "McGlinn2010","DelMoral2010",
                    "PortalMammals","Woods2009","Palmer2007",
                    "TreeWesternGhats")
temporalData=temporalData1[-c(1:3)]

# Exclude Adler data sets - only a subset of plant community sampled

# Install data sets as databases
for(i in 1:length(temporalData)){
  
  ecoretriever::install(dataset = paste0(temporalData[i]), 
                        connection = 'sqlite', 
                        db_file = paste0(temporalData[i],
                                         ".sqlite1"))
  
}


ecoretriever::install(dataset = paste0(temporalData[2]), 
                      connection = 'sqlite', 
                      db_file = paste0(temporalData[2],
                                       ".sqlite1"))

# Access one of the data sets using SQLite
db <- dbConnect(SQLite(), paste0(temporalData[2],".sqlite1"))

# List tables within the database
dbListTables(db)


# Format data =========================================================

# Merge relevant tables in R (faster) ----------------------
# Plot and species tables
plot_info       <- dbGetQuery(db, paste0("SELECT * FROM ", 
                                    dbListTables(db)[1]))
spp_abundance   <- dbGetQuery(db, paste0("SELECT * FROM ", 
                                    dbListTables(db)[3]))

# Remove non-existing data from plot table
plot_info <- subset(plot_info,plot_code!="")

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
                           VALUE = "long_degrees")

plot_lat   <- format_data(plot_info,
                           OBSERVATION_TYPE = "ENV_VAR", 
                           SITE_ID = "plot_code", 
                           DATE = NA, # NA because non-temporal
                           VARIABLE_NAME = "longitude", 
                           VARIABLE_UNITS = "dec.degrees",  
                           VALUE = "lat_degrees")

# Stack longitude and latitude together
plot_coord  <- rbind(plot_long,plot_lat)


# Species abundance data -------------------------------------------
spp_abundance <- rename(spp_abundance,VALUE = count,
                        SITE_ID = plot_code,
                        VARIABLE_NAME = species,
                        DATE = year)
spp_abundance <- mutate(spp_abundance, 
                        OBSERVATION_TYPE = "TAXON_ABUNDANCE",
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
                           VALUE = "elevation_m")

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
                        VALUE = "slope_degrees")

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
delmodal_data <- rbind(plot_coord,spp_abundance,env_vars)

write.csv(delmodal_data,"MtStHelens_succession.csv",row.names = F)
