## Code to clean the California Fish and Wildlife's Fish Survey Data
## NOTE: Full metadata is in a separate .csv on google drive
## NOTE: Current version includes jelly fish - remove?
## NOTE: Current version does not have lat/long - need to ask for it? 
## The website has an interactive map with the sites, so coordinates are definitely known...

library(tidyr)
library(dplyr)

rm(list = ls())

# key to the website from google drive
fishkey <- "0BwguSiR80XFZM3ZranBtczRrbjA" # Google Drive file ID

# clean the spp data
cdfw.spp <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", fishkey)) %>%
  tbl_df() %>%
  mutate(Stripped.Bass = Striped.Bass.age.0 + Striped.Bass.age.1 + Striped.Bass.age.2 + Striped.Bass.age.3.) %>%
  select(-c(Striped.Bass.age.0, Striped.Bass.age.1, Striped.Bass.age.2, Striped.Bass.age.3.)) %>%
  select(Year, Date, Station, Aequorea.spp.:Stripped.Bass) %>%
  tbl_df() %>%
  gather(VARIABLE_NAME, VALUE, Aequorea.spp.:Stripped.Bass) %>%
  mutate(SITE_ID = Station,
         OBSERVATION_TYPE = "TAXON_COUNT",
         VARIABLE_UNITS = "count",
         DATE = Date,
         VALUE = ifelse(is.na(VALUE), 0, VALUE)) %>%
  select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE)
  
# clean the environmental data
cdfw.env <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", fishkey)) %>%
  tbl_df() %>%
  mutate(MaxTemp = Top_TempC, 
         MaxEC = Top_EC_muS_cm,
         Secchi = Secchi_m.) %>%
  select(Date, Station, MaxTemp, MaxEC, Secchi) %>%
  gather(VARIABLE_NAME, VALUE, MaxTemp:Secchi) %>%
  mutate(VARIABLE_UNITS = "degrees C",
         VARIABLE_UNITS = ifelse(VARIABLE_NAME == "MaxEC", "muS per cm", VARIABLE_UNITS),
        VARIABLE_UNITS = ifelse(VARIABLE_NAME == "Secchi", "meters", VARIABLE_UNITS),
        DATE = Date,
        OBSERVATION_TYPE = "ENV_VAR")

cdfw.dat <- rbind(cdfw.spp, cdfw.env)
  