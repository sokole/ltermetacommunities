# this R script reads in a Cedar Creek dataset from the LTER repository
# converts it to the most flexible data model and writes it out with EML as datapackage

library(dplyr)
library(tidyr)
library(EML)

#set up most flexible precursor data model
#observations.csv : "observation_id", "event_id", "study_id", "sampling_location_id", "datetime", "taxon_id", "variable_name", "value", "unit"
#sampling_location.csv: "sampling_location_id", "sampling_location_name", "latitude", "longitude", "elevation", "parent_sampling_location_id"
#sampling_location_description.csv: "sampling_location_id", "datetime", "variable_name", "value", "unit"
#taxon.csv: "taxon_id", "taxon_level", "taxon_name", "authority_system", "authority_taxon_id"
#taxon_descr.csv: "taxon_id", "datetime", "variable_name", "value", "author"
#event_data.csv: "event_id", "variable_name", "value"
#study.csv: "study_id", "taxon_group", "ecological_group", "intent", "study_design", "methods", "original_dataset_id"

# get the data file from the LTER repository

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/386/5/34db2945b271c1a142d73fec2e298917"
infile1 <- sub("^https","http",infile1) 
dt1 <-read.csv(infile1, header=TRUE, sep="\t") 
tmpDateFormat<-"%m/%d/%Y"
dt1$Sample.Date<-as.Date(dt1$Sample.Date,format=tmpDateFormat)

#get corresponding EML file

f <- "https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-cdr.386.5&contentType=application/xml"
eml <- read_eml(f)

#put together observation table
#rename column headings to match
dt2 <- dt1
colnames(dt2) <- c("Year", "datetime", "plot","Heat.Treatment", "taxon_id", "value")
dt2 <- mutate(dt2, plot_id = paste("cdr386_5.", plot,".", Heat.Treatment, sep = ""))
dt2 <- mutate(dt2, variable_name = "biomass", unit = "gramsPerSquareMeter")
dt2 <- mutate(dt2, observation_id = row.names(dt2), event_id = "", study_id = "cdr386_5", sampling_location_id = plot_id)
df_observation <- select(dt2, observation_id, event_id, study_id, sampling_location_id, datetime, taxon_id, variable_name, value, unit)
write.csv(df_observation, file = "EDI/observations.csv")

#put together sampling location table
dt3 <- select(dt2, sampling_location_id, plot, Heat.Treatment)
dt3 <- unique(dt3)
dt4 <- dt3
dt3 <- mutate(dt3, sampling_location_name = plot, latitude = "", longitude = "", elevation = "", parent_sampling_location_id = "cdr_site")
df_sampling_location <- select(dt3, sampling_location_id, sampling_location_name, latitude, longitude, elevation, parent_sampling_location_id)

#add the site information, parent site 
dt3 <- data.frame("sampling_location_id" = "cdr_site",
                  "sampling_location_name" = "Cedar Creek LTER site",
                  "latitude" = "45.4131225",
                  "longitude" = "-93.19367",
                  "elevation" = "",
                  "parent_sampling_location_id" = "")
df_sampling_location <- rbind(df_sampling_location, dt3)

write.csv(df_sampling_location, file = "EDI/sampling_location.csv")

# put together sampling location description
dt4 <- mutate(dt4, datetime= "", variable_name = "heat treatment", value = Heat.Treatment, unit = "")
df_sampling_location_description <- select(dt4, sampling_location_id, datetime, variable_name, value, unit)
write.csv(df_sampling_location_description, file = "EDI/sampling_location_description.csv")
