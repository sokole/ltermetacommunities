# --------------------------------------------------------- #
# Format raw data as a list of tables - created 10 Oct 2016 #
# --------------------------------------------------------- #

# Clear environment
rm(list = ls())

# Assign data set of interest
# NOTE: Google Drive file ID is different for each dataset


# CAP LTER (Central Arizona-Phoenix)
#data.set <- "CAP-birds-CORE"
#data.key <- "0BzcCZxciOlWgeHJ5SWx1YmplMkE" # Google Drive file ID 

# NWT LTER (Niwot Ridge)
#data.set <- "NWT-plants-Hallett-and-Sokol"
#data.key <- "0B2P104M94skvQVprSnBsYjRzVms" # Google Drive file ID 

# SBC LTER (Santa Barbara Coastal)
data.set <- "SBC-Lamy-Castorani"
data.key <- "0BxUZSA1Gn1HZYTVfd2FZTWhWbm8" # Google Drive file ID 

# ---------------------------------------------------------------------------------------------------
# Set working environment
setwd(paste("~/Google Drive/LTER Metacommunities/LTER-DATA/", data.set, sep=""))

# Check for and install required packages
#library()

for (package in c('dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom', 'ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# ---------------------------------------------------------------------------------------------------
# IMPORT DATA
dat.long <-  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", data.key))

# MAKE DATA LIST
dat <- list()

# COMMUNITY DATA 
comm <- dat.long[dat.long$OBSERVATION_TYPE == "TAXON_COUNT", ]
comm <- droplevels(comm)
# Subset data if necessary
#dat$comm <- subset(comm, dat$comm$TAXON_GROUP != "INSERT NAME OF REMOVAL GROUP HERE")
#dat$comm <- droplevels(dat$comm)
str(comm)

#Add number of species to data list:
dat$n.spp <- length(levels(comm$VARIABLE_NAME))


# Ensure that site data is a character, not a string
class(comm$SITE_ID)
comm$SITE_ID <- as.character(dat$comm$SITE_ID)

# Ensure that VALUE is numeric
class(comm$VALUE)
comm$VALUE <- as.numeric(as.character(comm$VALUE))

#
# Balanced sampling of species across space and time? inspect table:
tapply(comm$VALUE, list(comm$SITE_ID,comm$DATE), length)

# Convert community data to wide form
  comm <- comm %>%
  select(-VARIABLE_UNITS) %>%
  select(-TAXON_GROUP) %>%
    tidyr::spread(VARIABLE_NAME,  VALUE)
    
dat$comm <- comm
str(dat)


# SPATIAL DATA
# Check for and install required packages

for (package in c('dplyr', 'tidyr', 'XML', 'sp', 'geosphere', 'rgdal','maps','reshape2','ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#pull out coordinate data and make sure that it is numeric
cord =filter(dat.long, OBSERVATION_TYPE=="SPATIAL_COORDINATE");head(cord)
cord$SITE_ID=toupper(cord$SITE_ID)
cord.wide = spread(cord,VARIABLE_NAME, VALUE);head(cord.wide) #create rows from lat long
sites=c(unique(cord.wide$SITE_ID));sites

# keep the records that are _not_ duplicated
cord.wide =subset(cord.wide, !duplicated(SITE_ID));dim(cord.wide)  # here we selcet rows (1st dimension) that are different from the object dups2 (duplicated records)
cord.wide
cord.wide$latitude <- as.numeric(as.character(cord.wide$LAT)) 
cord.wide$longitude <- as.numeric(as.character(cord.wide$LONG)) #cord.wide$longitude <- as.numeric(as.character(cord.wide$longitude))
cord.wide=cord.wide[c("longitude", "latitude")] #pull last two columns and reorder



#add number of sites and long/lat coords to data list:
dat$n.sites <- length(sites)
dat$longlat <- cord.wide


#make data spatially explicit
coordinates(cord.wide) = c("longitude", "latitude") #coordinates(cord.wide) <- c("longitude", "latitude") 
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(cord.wide) <- crs.geo  # define projection system of our data to WGS84 (CHECK TO SEE IF THIS WORKS IF SPATIAL COORDINATE IS NOT IN DEC.DEGREES)
summary(cord.wide) 

#if DATA IS IN UTM OR OTHER KNOWN COORDINATE SYSTEM YOU CAN TRANSFORM IT, EG... UTM data for PHX
#coordinates(cord.wide) <- c("X", "Y")
#crs.geo = CRS("+proj=utm +zone=12 ellps=WGS84")  # define coordinate reference system of your specific data
#proj4string(cord.wide) = crs.geo # define coordinate reference system of your specific data
#summary(cord.wide) 
#cord.wide <- spTransform(cord.wide, CRS("+proj=longlat")) #transform to latlog CRS
#summary(cord.wide) #check transformation

#create a distance matrix between sites, best fit distance function TBD
distancemat=(distm(cord.wide, fun=distVincentyEllipsoid)/1000);distancemat #km distance
rownames(distancemat) = sites
colnames(distancemat) = sites

#add distance matrix to data list
dat$distancemat <- distancemat
str(dat)

# ENVIRONMENTAL COVARIATES
env.long <- subset(dat.long, OBSERVATION_TYPE == "ENV_VAR")
env.long <- droplevels(env.long)
str(env.long)

levels(env.long$VARIABLE_UNITS)
levels(env.long$VARIABLE_NAME)
#convert to wide
  env.wide <- env.long %>%
  select(-VARIABLE_UNITS) %>%
    tidyr::spread(VARIABLE_NAME,  VALUE)

#add environmental covaiates to data list
dat$env <- env.wide
#NOTE: nrow in dat$comm should be the same as nrow in dat$env for future analysis (environmental covariates that are specific to each site-year)
nrow(dat$comm); nrow(dat$env)

#FILL IN SOME INFO ABOUT THE DATASET THAT WILL BE HELPFUL FOR LOOPS:
dat$n.years <- length(unique(dat$comm$DATE))


str(dat)
#write .Rdata object into the directory "intermediate_data"
setwd()
write(dat, file = paste(data.set,".Rdata", sep=""))
getwd()
#now, explore the data and perform further QA/QC with the script "2_explore_data.R"
