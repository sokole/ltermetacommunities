## Script to transform NTL Zooplankton data into the long format
## Authors: Timothy Nguyen and Julien Brun, NCEAS
## Contact: SciComp@nceas.ucsb.edu

source("utilities/import_tools.R") #tools containing function to read from Google drive

coord_file_hash <- "0B-HySt4HfBxBVjJFRWhIMTd4cVU"


ntl_coordinates_formater <- function(file_hash = coord_file_hash){
    # Read the file
    coord_data <- read_csv_gdrive(file_hash)
    # Format lat and long
    lat <- data.frame(OBSERVATION_TYPE = 'SPATIAL_COORDINATE',
                      SITE_ID = coord_data$lakeid,
                      DATE = NA,
                      VARIABLE_NAME = 'LATITUDE',
                      VARIABLE_UNITS = 'decimal.degrees',
                      VALUE = coord_data$Latitude)
    long <- data.frame(OBSERVATION_TYPE = 'SPATIAL_COORDINATE',
                       SITE_ID = coord_data$lakeid,
                       DATE = NA,
                       VARIABLE_NAME = 'LONGITUDE',
                       VARIABLE_UNITS = 'decimal.degrees',
                       VALUE = coord_data$Longitude)
    
    coordinates_df <- rbind(lat, long)
    return(coordinates_df)
}
