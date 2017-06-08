source("./NTL_data/import_tools.R") #tools containing function to read from Google drive

## Loading packages ----

library(tidyr)
library(dplyr)
library(devtools)
library(ggplot2)


## Constants ----

path <- "./NTL_data/NTL_Fish"
file_hash <- "0B7AABlvKD6WjSTY3YUVKZ1AwLWs"
output_file <- file.path(path, "NTL_Fish_long.csv")


## Functions ----

make_data_long <- function(data) {
  long <- data %>% 
    # Name the fields as required by the data format
    rename(EFFORT_COUNT = total_effort, TAXON_COUNT = total_caught, SITE_ID = lakeid, DATE = year4, VARIABLE_NAME = spname) %>%
    # Compute the catch per unit effort as # of fish caught / effort
    mutate(VALUE = TAXON_COUNT/EFFORT_COUNT) %>%
    # Removing the 
    select(-TAXON_COUNT,-EFFORT_COUNT) %>%
    #Adding the extra columns for observation type and unit
    mutate(OBSERVATION_TYPE = "TAXON_COUNT") %>%
    mutate(VARIABLE_UNITS = "CPUE")
  return(long)
}


## Main ----

# Read file from Google drive
my_data <- read_csv_gdrive(file_hash)

# Check the Number of years of measurement
nb_year <- length(unique(my_data$year4))

# Check is some specific gear is rarely used
my_data %>% 
  group_by(gearid) %>% 
  summarize(n = length(unique(year4))) 

# => Seems like the gill nets VGN and VGN127 are rarely used
## Remove these gill nets type from the data

# Check how they look like 
my_data %>% filter(gearid=="VGN" | gearid=="VGN127")

# Preparing the data for the reformatting and aggregating
my_df <- my_data %>%
  # remove gears that were not used in a systematic way (from phone call information)
  filter(gearid!="VGN" & gearid!="VGN127") %>%
  # Sum accross the gears for effort and number of fish caught
  group_by(year4, lakeid, spname) %>%
  summarise(total_caught = sum(total_caught), total_effort = sum(effort)) 

## transform the data into the long format
long_data <- make_data_long(my_df)

# write the ouput file
write.csv(long_data, file=output_file, row.names=FALSE)
