# this R script reads in a Cedar Creek dataset from the LTER repository
# converts it to the most flexible data model and writes it out with EML as datapackage

library(dplyr)
library(tidyr)
library(reshape2)
library(EML)
library(taxize)

#set up most flexible precursor data model
#observations.csv : "observation_id", "event_id", "study_id", "sampling_location_id", "datetime", "taxon_id", "variable_name", "value", "unit"
#sampling_location.csv: "sampling_location_id", "sampling_location_name", "latitude", "longitude", "elevation", "parent_sampling_location_id"
#sampling_location_description.csv: "sampling_location_id", "datetime", "variable_name", "value", "unit"
#taxon.csv: "taxon_id", "taxon_level", "taxon_name", "authority_system", "authority_taxon_id"
#taxon_descr.csv: "taxon_id", "datetime", "variable_name", "value", "author"
#event_data.csv: "event_id", "variable_name", "value"
#study.csv: "study_id", "length_of_survey_years", "number_of_years_sampled", "sampling_area", "number_of_taxa", "original_dataset_id"

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
dt2 <- mutate(dt2, plot_id = paste("cdr386.", plot,".", Heat.Treatment, sep = ""))
dt2 <- mutate(dt2, variable_name = "biomass", unit = "gramsPerSquareMeter")
dt2 <- mutate(dt2, observation_id = row.names(dt2), event_id = "", study_id = "cdr386", sampling_location_id = plot_id)
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

#put together the taxon table
dt5 <- select(dt1, Species)
species_list <- as.vector(unique(dt5$Species))
cleaned_species_list <- species_list

#clean up the taxon name a bit
for (i in 1:length(cleaned_species_list)){
  cleaned_species_list[i] <- sub(substr(cleaned_species_list[i], 1, 1),toupper(substr(cleaned_species_list[i], 1, 1)),cleaned_species_list[i])
  cleaned_species_list[i] <- gsub("\\(", " ", cleaned_species_list[i])
  cleaned_species_list[i] <- gsub("\\)", " ", cleaned_species_list[i])
  cleaned_species_list[i] <- gsub("sp.", " ", cleaned_species_list[i])
  cleaned_species_list[i] <- gsub(" sp", " ", cleaned_species_list[i])
  cleaned_species_list[i] <- gsub("  ", " ", cleaned_species_list[i])
  cleaned_species_list[i] <- trimws(cleaned_species_list[i])
}

#get as much information as possible from authorities
taxon_info <- classification(cleaned_species_list, db = 'itis')

#create the taxon table to hold the information
df_taxon <- data.frame(matrix(nrow = 0, ncol = 5))
col_names <- c("name", "rank", "id", "taxon_name", "authority_system")
colnames(df_taxon) <- col_names

for (i in 1:length(species_list)) {
  if (length(taxon_info[[i]]) > 1) {
    d <- nrow(melt(taxon_info[i]))
    taxon_record <- as.data.frame(slice(melt(taxon_info[i]), d))
    taxon_record <- select(taxon_record, name, rank, id)
    taxon_record$name <- species_list[i]
    taxon_record <- mutate(taxon_record, taxon_name = cleaned_species_list[i], authority_system = "https://www.itis.gov/")
  }else{
    taxon_record <- data.frame("name" = species_list[i],
                    "rank" = "",
                    "id" = "",
                    "taxon_name" = cleaned_species_list[i],
                    "authority_system" = "")
  }
  df_taxon <- rbind(df_taxon, taxon_record)
}

#taxon.csv: "taxon_id", "taxon_level", "taxon_name", "authority_system", "authority_taxon_id"

df_taxon <- mutate(df_taxon, taxon_id = name, taxon_level = rank, authority_system = "https://www.itis.gov/", authority_taxon_id = id)
df_taxon <- select(df_taxon, taxon_id, taxon_level, taxon_name, authority_system, authority_taxon_id)
write.csv(df_taxon, file = "EDI/taxon.csv")
