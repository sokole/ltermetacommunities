# this R script reads in a Cedar Creek dataset from the LTER repository
# converts it to the most flexible data model and writes it out with EML as datapackage

library(dplyr)
library(tidyr)
library(EML)

#set up most flexible precursor data model
df_observation <- data.frame(matrix(ncol = 9, nrow = 0))
col_names <- c("observation_id", "event_id", "study_id", "sampling_location_id", "datetime", "taxon_id", "variable_name", "value", "unit")
colnames(df_observation) <- col_names

df_sampling_location <- data.frame(matrix(ncol = 6, nrow = 0))
col_names <- c("sampling_location_id", "sampling_location_name", "latitude", "longitude", "elevation", "parent_sampling_location_id")
colnames(df_sampling_location) <- col_names

df_sampling_location_description <- data.frame(matrix(ncol = 5, nrow = 0))
col_names <- c("sampling_location_id", "datetime", "variable_name", "value", "unit")
colnames(df_sampling_location_description) <- col_names

df_taxon <- data.frame(matrix(ncol = 5, nrow = 0))
col_names <- c("taxon_id", "taxon_level", "taxon_name", "authority_system", "authority_taxon_id")
colnames(df_taxon) <- col_names

df_taxon_descr <- data.frame(matrix(ncol = 5, nrow = 0))
col_names <- c("taxon_id", "datetime", "variable_name", "value", "author")
colnames(df_taxon_descr) <- col_names

df_event_data <- data.frame(matrix(ncol = 3, nrow = 0))
col_names <- c("event_id", "variable_name", "value")
colnames(df_event_data) <- col_names

df_study <- data.frame(matrix(ncol = 7, nrow = 0))
col_names <- c("study_id", "taxon_group", "ecological_group", "intent", "study_design", "methods", "original_dataset_id")
colnames(df_study) <- col_names

# get the data file from the LTER repository

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/386/5/34db2945b271c1a142d73fec2e298917"
infile1 <- sub("^https","http",infile1) 
dt1 <-read.csv(infile1, header=TRUE, sep="\t") 
tmpDateFormat<-"%m/%d/%Y"
dt1$Sample.Date<-as.Date(dt1$Sample.Date,format=tmpDateFormat)

#get corresponding EML file

f <- "https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-cdr.386.5&contentType=application/xml"
eml <- read_eml(f)
