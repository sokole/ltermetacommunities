# --------------------------------------------------------- #
# Format raw data from L0 into L3 format                    #
# handy code TEMPLATE                                       #
# Revised 15 May 2018 by ERS                                #
# --------------------------------------------------------- #


# Contributors: Riley Andrade, Max Castorani, Nina Lany, Sydne Record, Nicole Voelker
# revised by Eric Sokol

#Here is some template or example code that may be helpful for putting datasets into the format for the LTER metacommunities working group synthesis project and performing a few data checks. 
#Make one script for each dataset that converts the data from the form we originally got it in to the one we need. This might involve aggregating subplots and/or multiple sampling occasins by year, subsetting out sites or species, etc. It may also be necessary to fill in an abundance of zero for any taxa that appear in the dataset but were not observed at a particular site-year.
#Save the resulting file in the directory "~/Google Drive/LTER Metacommunities/LTER-DATA/L3-aggregated-by-year-and-space". This file should be ready for preliminary analalysis.
#The next step in the pipeline after this one is to run each dataset through the script 3_explore_comm_dat.R. This script plots the sampling effort (to be sure overvations are balanced), the species accumulation curve, and a time series of species richness at each site as well as in aggragate. If these preliminiary visualizations turn up something problematic, go back to the script spcific to the dataset and add in annotated code to fix the problem.
 
options(stringsAsFactors = FALSE)

# Clear environment
rm(list = ls())

# user vars
data_product_directory_name <- 'VCR-96plantDuneBiomass-popler'


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
ecocom_dp_dir <- googledrive::drive_ls(paste0('~/LTER Metacommunities/LTER-DATA/L0-raw/',
                                              data_product_directory_name,
                                              '/ecocomDP_export/'))

# ---------------------------------------------------------------------------------------------------
#IMPORT AND FORMAT DATA FROM EDI
#Some data will come from the EDI portal in the ecocommDP format. Here is code that downloads an ecocommDP dataset diretly from the EDI portaland converts it into the necessary format.

#1. read in flat tables from EDI. The code below was modified from the code that is available on the EDI page for each dataset (look for an 'import data from R' link). Once you obtain this code for the dataset of interest, the rest of the steps should run without modification. Be sure to add 'stringsAsFactors = F' to the code that imports each csv.

#get google_id for Observation table
google_id <- ecocom_dp_dir %>% filter(grepl('observation',name)) %>% select(id) %>% unlist()
dt1 <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id))

# #Sampling location ancillary table
# google_id <- ecocom_dp_dir %>% filter(grepl('location',name)) %>% select(id) %>% unlist()
# dt2 <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id))

# #dataset summary table
# infile3  <- "https://pasta.lternet.edu/package/data/eml/edi/124/3/35e680b74d58817276e416d4de63f447" 
# infile3 <- sub("^https","http",infile3) 
#  dt3 <-read.csv(infile3,header=F 
#           ,skip=1
#             ,sep="\t"  
#                 ,quot='"' 
#         , col.names=c(
#                     "package_id",     
#                     "original_package_id",     
#                     "length_of_survey_years",     
#                     "number_of_years_sampled",     
#                     "std_dev_interval_betw_years",     
#                     "max_num_taxa",     
#                     "geo_extent_bounding_box_m2"    ), check.names=TRUE,
#                     stringsAsFactors = F)
               
  
#sampling location table      
google_id <- ecocom_dp_dir %>% filter(grepl('location',name)) %>% select(id) %>% unlist()
dt4 <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id))

#taxon table
google_id <- ecocom_dp_dir %>% filter(grepl('taxon',name)) %>% select(id) %>% unlist()
dt5 <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id))

#2 Make TAXON_COUNT from the Observation table (dt1)
#select the columns of interest
dat1 <- dt1 %>% 
  rename(SITE_ID = location_id, 
         DATE = observation_datetime,
         VARIABLE_NAME = taxon_id,
         VARIABLE_UNITS = variable_name,
         VALUE = value) %>% 
  mutate(OBSERVATION_TYPE = 'TAXON_COUNT') %>%
  select(OBSERVATION_TYPE,SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE)


#3 Make SPATIAL_COORDINATE from the sampling location table (dt4)
#select the columns of interest and convert to long 
dat4 <- data.frame()
dat4 <- dt4 %>% gather('VARIABLE_NAME', 'VALUE', latitude, longitude) %>%
  rename(SITE_ID = location_id) %>%
  mutate(OBSERVATION_TYPE = 'SPATIAL_COORDINATE',
         VARIABLE_UNITS = 'dec. degrees',
         DATE = NA) %>% 
  select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE)


#4 Combine into one big table
# dat.long <- rbind(dat1, dat4)

# #5 perform a few checks against dataset summary table (dt3)
# ifelse(length(unique(dat1$VARIABLE_NAME))==dt3$max_num_taxa,
#   "OK: Number of taxa matches dataset summary table.",
#   "ERROR:Number of taxa does not match dataset summary table.")

# date <- as.POSIXlt(dat1$DATE, format = "%Y-%m-%d")
# year <- as.numeric(format(date, "%Y"))
# ifelse(length(unique(year))== dt3$number_of_years_sampled, 
#   "OK: Number of years sampled matches dataset summary table.",
#   "ERROR:Number of years sampled does not match dataset summary table.")

# ifelse( max(year)-min(year) + 1 == dt3$length_of_survey_years, 
#   "OK: Length of survey matches dataset summary table.",
#   "ERROR:Length of survey does not match dataset summary table.")

#

# MAKE DATA LIST
dat <- list()

# COMMUNITY DATA 
comm.long <- dat1

# checking for weird taxa names and removing suspicious ones that are rare
comm.long_gamma_summary <- comm.long %>% group_by(VARIABLE_NAME) %>% 
  filter(!is.na(VARIABLE_NAME) & VALUE>0) %>%
  summarize(mean_value = mean(VALUE, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(RA = mean_value / sum(mean_value))
         
comm.long_gamma_summary_remove_unk <- comm.long_gamma_summary %>% 
  filter(!grepl('(?i)unk',VARIABLE_NAME))

if(!nrow(comm.long_gamma_summary) == nrow(comm.long_gamma_summary_remove_unk)){
  message('WARNING: suspicous taxa removed -- taxaID had "unk"')
}


# Subset data if necessary
#comm.long <- subset(comm.long, comm.long$TAXON_GROUP != "INSERT NAME OF REMOVAL GROUP HERE")
#comm.long <- droplevels(comm.long)
str(comm.long)  # Inspect the structure of the community data

#Add number of unique taxa and number of years to data list:
dat$n.spp <- length(levels(comm.long$VARIABLE_NAME))

# Ensure that community data VALUE and DATE are coded as numeric
# if more than one date per year, aggregate by taking mean counts/biomass/cover across bouts per year.
comm_sampling_summary_table <- comm.long %>%
  # group_by(SITE_ID, DATE) %>%
  # summarize(n_bouts_year = length(unique(DATE)))
  mutate(YEAR = lubridate::year(DATE)) %>%
  group_by(SITE_ID, YEAR) %>%
  summarize(n_bouts_year = length(unique(DATE))) %>%
  rename(DATE = YEAR)

if(length(unique(comm_sampling_summary_table$n_bouts_year)) > 1){
  message('WARNING: different number of sampling bouts in each different years!')
  print(comm_sampling_summary_table)
}

dat$comm_sampling_summary_table <- comm_sampling_summary_table

comm.long <- comm.long %>%   # Recode if necessary
  mutate(DATE = lubridate::year(DATE)) %>%
  mutate_at(vars(c(DATE, VALUE)), as.numeric) %>%
  group_by(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS) %>%
  summarize(VALUE = mean(VALUE, na.rm = TRUE),
            n_bouts = length(DATE)) %>%
  select(-n_bouts) %>%
  ungroup()

dat$n.years <- length(unique(comm.long$DATE))

# Ensure that community character columns coded as factors are re-coded as characters
comm.long <- comm.long %>%   # Recode if necessary
  mutate_if(is.factor, as.character)
  
# Ensure that SITE_ID is a character: recode numeric as character 
comm.long <- comm.long %>%   # Recode if necessary
  mutate_at(vars(SITE_ID), as.character)

# Double-check that all columns are coded properly
ifelse(FALSE %in% 
   c(
     class(comm.long$OBSERVATION_TYPE) == "character",
     class(comm.long$SITE_ID) == "character",
     class(comm.long$DATE) == "numeric",
     class(comm.long$VARIABLE_NAME) == "character",
     class(comm.long$VARIABLE_UNITS) == "character",
     class(comm.long$VALUE) == "numeric"
     #class(comm.long$TAXON_GROUP) == "character")
   ),
  "ERROR: Community columns incorrectly coded.", 
  "OK: Community columns correctly coded.")

# ---------------------------------------------------------------------------------------------------
# Check balanced sampling of species across space and time by inspecting table, and add to data list

# make wide, fill with 0, then back to long to propagate zeros
if(length(unique(xtabs(~ SITE_ID + DATE, data = comm.long))) > 1){
  comm.long <- comm.long %>%
    spread(VARIABLE_NAME, VALUE, fill = 0) %>%
    gather('VARIABLE_NAME','VALUE', -c(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_UNITS))
}

xtabs(~ SITE_ID + DATE, data = comm.long)
hist(na.omit(comm.long$DATE))

ifelse(length(unique(xtabs(~ SITE_ID + DATE, data = comm.long))) == 1,
       "OK: Equal number of taxa recorded across space and time.", 
       "ERROR: Unequal numbers of observations across space and time, or taxa list not fully propagated across space and time. Inspect contingency table.")

# ---------------------------------------------------------------------------------------------------
# Add to dat list the unique taxa
dat$comm.long <- comm.long

# Convert community data to wide form
comm.wide <- comm.long %>%
  select(-VARIABLE_UNITS) %>%
  spread(VARIABLE_NAME,  VALUE)

dat$comm.wide <- comm.wide
summary(dat)

dat.long <- comm.long

# ---------------------------------------------------------------------------------------------------
# SPATIAL DATA
# Check for and install required packages
if( nrow(dat4) > 0 & !(dat4$VALUE %>% anyNA()) ){
  for (package in c('dplyr', 'tidyr', 'XML', 'sp', 'geosphere', 'rgdal','maps','reshape2','ggplot2')) {
    if (!require(package, character.only=T, quietly=T)) {
      install.packages(package)
      library(package, character.only=T)
    }
  }
  
  dat.long <- bind_rows(dat.long, dat4)
  
  #pull out coordinate data and make sure that it is numeric
  cord <- filter(dat.long, OBSERVATION_TYPE=="SPATIAL_COORDINATE");head(cord)
  cord$SITE_ID <- toupper(cord$SITE_ID)  # Ensure sites are in all caps
  cord <- droplevels(cord)
  str(cord)
  
  cord.wide <- cord %>%
    select(-VARIABLE_UNITS) %>%
    spread(VARIABLE_NAME,  VALUE)
  
  head(cord.wide)
  
  sites <- c(unique(cord.wide$SITE_ID));sites
  
  # keep the records that are _not_ duplicated
  cord.wide <- subset(cord.wide, !duplicated(SITE_ID));dim(cord.wide)  # here we selcet rows (1st dimension) that are different from the object dups2 (duplicated records)
  cord.wide
  cord.wide$latitude <- as.numeric(as.character(cord.wide$LAT)) 
  cord.wide$longitude <- as.numeric(as.character(cord.wide$LONG)) #cord.wide$longitude <- as.numeric(as.character(cord.wide$longitude))
  cord.wide <- cord.wide[c("longitude", "latitude")] #pull last two columns and reorder
  
  #add number of sites and long/lat coords to data list:
  dat$n.sites <- length(sites)
  dat$longlat <- cord.wide
  
  #make data spatially explicit
  coordinates(cord.wide) = c("longitude", "latitude") #coordinates(cord.wide) <- c("longitude", "latitude") 
  crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # SBC
  crs.geo <- CRS("+proj=utm +zone=13 +datum=WGS84") #NWT, PHX=zone12
  proj4string(cord.wide) <- crs.geo  # define projection system of our data to WGS84 (CHECK TO SEE IF THIS WORKS IF SPATIAL COORDINATE IS NOT IN DEC.DEGREES)
  summary(cord.wide) 
  
  #if DATA IS IN UTM OR OTHER KNOWN COORDINATE SYSTEM YOU CAN TRANSFORM IT, EG... UTM data for PHX and NWT 
  cord.wide <- spTransform(cord.wide, CRS("+proj=longlat")) 
  summary(cord.wide) #check transformation
  
  #create a distance matrix between sites, best fit distance function TBD
  distance.mat <- (distm(cord.wide, fun = distVincentyEllipsoid)/1000);distance.mat #km distance
  rownames(distance.mat) <- sites
  colnames(distance.mat) <- sites
  
  #add distance matrix to data list
  dat$distance.mat <- distance.mat
  summary(dat)
}else{
  
  dat$n.sites <- dat.long$SITE_ID %>% unique() %>% length()
  message('WARNING: spatial data missing')
  
}


# ---------------------------------------------------------------------------------------------------
# ENVIRONMENTAL COVARIATES
if( 'ENV_VAR'%in%dat.long$OBSERVATION_TYPE & !(dat.long %>% filter(OBSERVATION_TYPE == 'ENV_VAR'))$VALUE %>% anyNA() ){
  
  env.long <- subset(dat.long, OBSERVATION_TYPE == "ENV_VAR")
  env.long <- droplevels(env.long)
  str(env.long)
  
  # Convert from long to wide
  env.wide <- env.long %>%
    select(-VARIABLE_UNITS) %>%
    tidyr::spread(VARIABLE_NAME,  VALUE)
  
  # Add environmental covaiates to data list
  
  dat$n.covariates <- length(levels(env.long$VARIABLE_NAME))
  dat$cov.names <- levels(env.long$VARIABLE_NAME)
  dat$env <- env.wide
  dat$env.long <- env.long
  
  #CHECK: 
  # Are all year-by-site combinations in community data matched by environmental data?
  ifelse(nrow(dat$comm.wide) == nrow(dat$env), "Yes", "No")
  
}else{
  message('WARNING: environmental data missing')
}




# Are community data balanced over space and time?
ifelse(nrow(dat$comm.wide) == dat$n.years * dat$n.sites, "Yes", "No")
# Inspect summary of 'dat' list 
summary(dat)

# #clean up the workspace
# try({
#   rm("comm.long","comm.wide","cord","cord.wide","crs.geo","dat.long", "data.key", "data.set","distance.mat","env.long", "env.wide","package","sites")
#   ls()
# })


# Now, explore the data and perform further QA/QC by sourcing this script within the scripts "2_explore_spatial_dat.R", "3_explore_comm_dat.R", and "4_explore_environmental_dat.R"

# Write CSV file for cleaned data (L3)
# write.csv(out_long, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-hbr-birds-sillett.csv", row.names = F)
write_path <- '~/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/'
drive_ls(write_path)

write_filename <- paste0('L3-',tolower(data_product_directory_name),'.csv')

# temp write local
readr::write_csv(dat.long, write_filename)
drive_upload(write_filename, 
             path = write_path, 
             name = write_filename, 
             type = NULL,
             verbose = TRUE)

#remove local file
file.remove(write_filename)

