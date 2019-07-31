# --------------------------------------------------------- #
# Format raw data from L0 into L3 format                    #
# sev-plants-compagnoni                                     #
# Revised 15 May 2018 by ERS                                #
# --------------------------------------------------------- #


# Contributors: Riley Andrade, Max Castorani, Nina Lany, Sydne Record, Nicole Voelker
# revised by Eric Sokol


options(stringsAsFactors = FALSE)

# Clear environment
rm(list = ls())

# user vars
#data_product_directory_name <- 'SEV-53pinonJuniper-popler'


#Check to make sure working directory is set to the ltermetacommunities github
if(basename(getwd())!="ltermetacommunities"){cat("Plz change your working directory. It should be 'ltermetacommunities'")}

# Check for and install required packages
for (package in c('googledrive','dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom', 'ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#set up googe drive for R
#ecocom_dp_dir <- googledrive::drive_ls(paste0('~/LTER Metacommunities/LTER-DATA/L0-raw/',
                                              data_product_directory_name,
                                              '/ecocomDP_export/'))

# ---------------------------------------------------------------------------------------------------
#IMPORT AND FORMAT DATA FROM GOOGLE DRIVE - via popler:

#get google_id for Observation table
#google_id <- ecocom_dp_dir %>% filter(grepl('observation',name)) %>% select(id) %>% unlist()
#dt1 <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id))
             
  
#sampling location table      
#google_id <- ecocom_dp_dir %>% filter(grepl('location',name)) %>% select(id) %>% unlist()
#dt4 <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id))

#taxon table
#google_id <- ecocom_dp_dir %>% filter(grepl('taxon',name)) %>% select(id) %>% unlist()
#dt5 <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id))



# ---------------------------------------------------------------------------------------------------
#IMPORT AND FORMAT DATA FROM EDI

# Package ID: knb-lter-sev.278.245672 Cataloging System:https://pasta.lternet.edu.
# Data set title: Pinon-Juniper (Core Site) Quadrat Data for the Net Primary Production Study at the Sevilleta National Wildlife Refuge, New Mexico (2003-present ).
# Data set creator:  Esteban Muldavin - SEV LTER 
# Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
# Contact:  Information Manager Sevilleta LTER -  SEV LTER  - data-use@sevilleta.unm.edu
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-sev.278.245672
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

# read directly from EDI
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sev/278/245672/43eb46261b04ab1e5d250b86ebf7fc35" 
infile1 <- sub("^https","http",infile1) 
dt1     <-read.csv(infile1,header=F,skip=1,sep=",", 
                   col.names=c( "year",     
                                "season",     
                                "date",     
                                "site", 
                                "plot",     
                                "transect",     
                                "quad",
                                "treatment",     
                                "species",     
                                "obs",     
                                "cover",     
                                "height",     
                                "count",     
                                "comments"), 
                                check.names=TRUE,
                                stringsAsFactors = F)

# function to remove NAs (-888)
remove_999 <- function(x) replace(x, x == -999, NA)
      
# format sev plants
sev <- dt1 %>% 
          # remove season 1 (winter, not always sampled)
          subset( !(season %in% 1) ) %>% 
          # remove lines with -999
          subset( !(count %in% -999) ) %>% 
          subset( !(species %in% -999) ) %>% 
          # remove plots connected to different transects (A, B, C, D)
          subset( !(plot %in% 3) ) %>% 
          mutate( DATE = year ) %>% 
          mutate( DATE = as.numeric(DATE) ) %>% 
          group_by( DATE, site, plot, transect, species) %>% 
          summarise( VALUE = sum(count,na.rm=T) ) %>% 
          ungroup() %>% 
          mutate( SITE_ID = paste(site,plot,transect, sep='_') ) %>% 
          rename( VARIABLE_NAME   = species ) %>% 
          mutate( VARIABLE_UNITS  = 'count') %>% 
          mutate(OBSERVATION_TYPE = 'TAXON_COUNT') %>%
          select(OBSERVATION_TYPE,
                 SITE_ID, DATE, VARIABLE_NAME, 
                 VARIABLE_UNITS, VALUE) %>% 
          # introduce zeros (if need be!)
          spread(VARIABLE_NAME, VALUE, fill = 0) %>% 
          gather(VARIABLE_NAME, VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS) %>% 
          unique

# write it out
write.csv(sev, '~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-sev-plants-compagnoni.csv', row.names=F)
 
 
 


### not data_product_directory_name is not the same as L3 data name here.
# Write CSV file for cleaned data (L3)
#write_path <- '~/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/'
#drive_ls(write_path)

#write_filename <- paste0('L3-',tolower(data_product_directory_name),'.csv')

# temp write local
#readr::write_csv(dat.long, write_filename)
#drive_upload(write_filename, 
#             path = write_path, 
#             name = write_filename, 
#             type = NULL,
#             verbose = TRUE)

#remove local file
#file.remove(write_filename)

