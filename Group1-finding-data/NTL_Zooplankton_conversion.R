## Script to transform NTL Zooplankton data into the long format
## Authors: Timothy Nguyen and Julien Brun, NCEAS
## Contact: SciComp@nceas.ucsb.edu

## -- LOADING PACKAGES --
source("Group1-finding-data/NTL_coordinates.R") #tools containing function to read from Google drive
library(tidyr)
library(dplyr)


## -- CONSTANTS --
input_file_hash <- '0B7AABlvKD6WjM1Z2TlFlVjI4cmM'
output <- 'NTL_Macroinvertebrates_long.csv'


## -- MAIN --

# Read file from Google drive
zoop_data <- read_csv_gdrive(input_file_hash)

agg_density <- zoop_data %>%
  tbl_df() %>%
  group_by(lakeid, year4, species_name) %>%
  summarise(agg_density=mean(density)) %>%
  rename(DATE=year4, VARIABLE_NAME=species_name, VALUE=agg_density, SITE_ID=lakeid) %>%
  data.frame()

n <- dim(agg_density)[1]
OBSERVATION_TYPE <- rep("TAXON_COUNT", n)
VARIABLE_UNITS <- rep("density", n) # this is too vague but I can't specify with the data I was given

my_zoop_long <- agg_density %>%
  cbind(OBSERVATION_TYPE, VARIABLE_UNITS) %>% # adding columns
  select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE)

head(my_zoop_long)

# Bring the coordinates
coord_df <- ntl_coordinates_formater(coord_file_hash)

# combine the two dataframes
zoop_output <- rbind(coord_df,my_macroinv_long)

# write the ouput file
write.csv(zoop_output, file=output, row.names=FALSE)
