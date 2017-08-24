## Script to transform NTL Macroinvertebrates data into the long format
## Authors: Timothy Nguyen and Julien Brun, NCEAS
## Contact: SciComp@nceas.ucsb.edu


## -- LOADING PACKAGES --
source("Group1-finding-data/NTL_coordinates.R") #tools containing function to read from Google drive
library(tidyr)
library(dplyr)


## -- CONSTANTS --
input_file_hash <- '0B7AABlvKD6WjZkRoVDYta1N6bFk'
output <- 'NTL_Macroinvertebrates_long.csv'


## -- MAIN --
# Read file from Google drive
data <- read_csv_gdrive(input_file_hash)

# Aggregating values
lake_cpue <- data %>%
  tbl_df() %>%
  group_by(lakeid, year4, taxon) %>%
  summarise(CPUE=mean(number_indiv)) %>%
  rename(DATE=year4, VARIABLE_NAME=taxon, VALUE=CPUE, SITE_ID=lakeid) %>%
  data.frame()

# Adding missing columns to complee long format
n <- dim(lake_cpue)[1]
OBSERVATION_TYPE <- rep("TAXON_COUNT", n)
VARIABLE_UNITS <- rep("count per unit effort", n)

my_macroinv_long <- lake_cpue %>%
  cbind(OBSERVATION_TYPE, VARIABLE_UNITS) %>% # adding columns
  select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE)

head(my_macroinv_long)

# Bring the coordinates
coord_df <- ntl_coordinates_formater(coord_file_hash)

# combine the two dataframes
macroinv_output <- rbind(coord_df,my_macroinv_long)

# write the ouput file
write.csv(macroinv_output, file=output, row.names=FALSE)

