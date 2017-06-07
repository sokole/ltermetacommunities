# this R script reads in a Cedar Creek dataset from the LTER repository
# converts it to the most flexible data model and writes it out with EML as datapackage

library(dplyr)
library(tidyr)
library(EML)

#set up most flexible data model



# get the data file from the LTER repository

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/386/5/34db2945b271c1a142d73fec2e298917"
infile1 <- sub("^https","http",infile1) 
dt1 <-read.csv(infile1, header=TRUE, sep="\t") 
tmpDateFormat<-"%m/%d/%Y"
dt1$Sample.Date<-as.Date(dt1$Sample.Date,format=tmpDateFormat)

#get corresponding EML file

f <- "https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-cdr.386.5&contentType=application/xml"
eml <- read_eml(f)
