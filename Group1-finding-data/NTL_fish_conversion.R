source("utilities/import_tools.R") #folder containing function to read from Google drive

## Loading packages ----
library(tidyr)
library(dplyr)
library(devtools)


## Constants ----

path <- "Group1-finding-data/"
file_hash <- "0B7AABlvKD6WjSTY3YUVKZ1AwLWs"
output_file = "NTL_fish_withgear.csv"

## Functions ----

make_data_long <- function(data) {
  long <- data %>% 
    # Name the fields as required by the data format
    rename(EFFORT_COUNT=effort,TAXON_COUNT=total_caught, SITE_ID=lakeid, DATE=year4, VARIABLE_NAME = spname) %>%
    # Computer the catch per unit effort as # of fish caught / effort
    mutate(VALUE = TAXON_COUNT/EFFORT_COUNT) %>%
    select(-TAXON_COUNT,-EFFORT_COUNT)
    #Adding the extra columns for observation type and unit
    result <- cbind(OBSERVATION_TYPE =(rep("TAXON_COUNT",length(long$DATE))), long, 
                  VARIABLE_UNITS = (rep("CPUE",length(long$DATE)))) 
    return(result)
}


## Main ----

# Read file from Google drive
my_df <- read_csv_gdrive(file_hash)

# Check the Number of years of measurement
nb_year <- length(unique(my_df$year4))

# Check is some specific gear is rarely used
my_df %>% 
  group_by(gearid) %>% 
  summarize(n = length(unique(year4))) 
# => Seems like the gill nets VGN and VGN127 are rarely used


## Remove these gill nets type from the data
# Check how they look like 
my_df %>% filter(gearid=="VGN" | gearid=="VGN127")

# Filter out
my_df <- my_df %>%
  filter(gearid!="VGN" & gearid!="VGN127")


## transform the data into the long format
long_data <- make_data_long(my_df)

## Quick look at the time-series per gear
ggplot(data=long_data, aes(x=DATE, y=VALUE, group = gearid, colour = gearid)) +
  +     geom_line()

# write the ouput file
write.csv(long_data, file=file.path(path,output_file), row.names=FALSE)
