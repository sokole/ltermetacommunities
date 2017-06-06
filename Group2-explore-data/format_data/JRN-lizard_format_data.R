# --------------------------------------------------------- #
# Format raw data as a list of tables                       #
# JORNADA lizard data                                                      # Riley Andrade (main contact)
# Revised 01 Jun 2017                                       #
# --------------------------------------------------------- #

# Contributors: Riley Andrade, Max Castorani, Nina Lany, Sydne Record, Nicole Voelker

# Clear environment
rm(list = ls())

# Set your working environment to the GitHub repository, e.g.: 
setwd("~/Documents/ltermetacommunities")

#Check to make sure working directory is correct
if(basename(getwd())!="ltermetacommunities"){cat("Plz change your working directory. It should be 'ltermetacommunities'")}

# Check for and install required packages
for (package in c('dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom', 'ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# ---------------------------------------------------------------------------------------------------

# Assign data set of interest
# NOTE: Google Drive file ID is different for each dataset

# JRN LTER (Jornada lizards from Andrew Hope )
data.set <- "JRN-lizard"
data.key <- "0BwguSiR80XFZa0NDckg5RmoyQVk" # Google Drive file ID 

data <-  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", data.key))

#look at data
str(data);unique(data$OBSERVATION_TYPE); unique(data$VARIABLE_NAME); 
#multiple counts per year; daily precipitation data. 
data1 <- filter(data, OBSERVATION_TYPE=="SPATIAL_COORDINATE")#subset site coordinates to add back onto frame after averaging

#Prepare year column for calculating averages
data$Date <- as.POSIXct(data$DATE, format = "%m/%d/%y")
data$Year <- as.numeric(format(data$Date,"%Y"))
#other cleaning
data$SITE_ID <- as.character(data$SITE_ID)
data$SITE_ID[data$SITE_ID == "G-IBP "] <- "G-IBPE" #noticed an error in sites names, fixing to keep consistent when comparing env vs species data


avgdata <-  filter(data, OBSERVATION_TYPE!="SPATIAL_COORDINATE") #subset to only include dated rows that need to be averaged

#number of species count and precip values per year per site
n.obs=aggregate(VALUE ~ VARIABLE_NAME+SITE_ID +Year, data=avgdata, length)
#So, take maximum of count data but sum of 365 days of precip data (annual precip) for each site-year:
commdata <-  filter(data, OBSERVATION_TYPE=="TAXON_COUNT")
agg_c=aggregate(VALUE ~ OBSERVATION_TYPE + SITE_ID + Year + VARIABLE_NAME + VARIABLE_UNITS, data=commdata, max, na.rm=TRUE)
data2=data.frame(agg_c)
names(data2)[names(data2)=="Year"]="DATE"

envdata <-  filter(data, OBSERVATION_TYPE=="ENV_VAR")
agg_e=aggregate(VALUE ~ OBSERVATION_TYPE + SITE_ID + Year + VARIABLE_NAME + VARIABLE_UNITS, data=envdata, sum, na.rm=TRUE)
data3=data.frame(agg_e)
names(data3)[names(data3)=="Year"]="DATE"


JRNdata <- rbind(data2, data1, data3) #bind data frames back together
levels(JRNdata$VARIABLE_UNITS)
levels(JRNdata$VARIABLE_NAME)
#write.csv(JRNdata,"JRNdata_RA.csv")

JRNdata1=subset(JRNdata,  ! SITE_ID %in% c("G-SUMM","M-NORT","C-IBPE", "G-GRAV", "P-COLL","P-SMAL","P-TOBO","T-TAYL") ) #removing sites that were not sampled for species data or unevenly sampled identified after 1st iteration of QA code

#write.csv(JRNdata1,"JRNdata_RA2.csv")

# --------------------------------------------------------- #
# Format raw data as a list of tables                       #
#                                                           #
# Revised 30 Mar 2017                                       #
# --------------------------------------------------------- #

# Contributors: Riley Andrade, Max Castorani, Nina Lany, Sydne Record, Nicole Voelker

# ---------------------------------------------------------------------------------------------------
#to match naming system
dat.long=JRNdata1

# MAKE DATA LIST
dat <- list()

# COMMUNITY DATA 
comm.long <- dat.long[dat.long$OBSERVATION_TYPE == "TAXON_COUNT", ] 
comm.long <- comm.long %>%
  droplevels()

str(comm.long)  # Inspect the structure of the community data

#Add number of unique taxa and number of years to data list:
dat$n.spp <- length(levels(comm.long$VARIABLE_NAME));dat$n.spp
dat$n.years <- length(unique(comm.long$DATE));dat$n.years

# Ensure that community data VALUE and DATE are coded as numeric
comm.long <- comm.long %>%   # Recode if necessary
  mutate_at(vars(c(DATE, VALUE)), as.numeric)

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

# ---------------------------------------------------------------------------------------------------
# SPATIAL DATA
# Check for and install required packages

for (package in c('dplyr', 'tidyr', 'XML', 'sp', 'geosphere', 'rgdal','maps','reshape2','ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#pull out coordinate data and make sure that it is numeric
cord <- filter(dat.long, OBSERVATION_TYPE=="SPATIAL_COORDINATE")
cord$SITE_ID <- toupper(cord$SITE_ID)  # Ensure sites are in all caps
cord <- droplevels(cord)
cord

cord.wide <- cord %>%
  select(-VARIABLE_UNITS) %>%
  spread(VARIABLE_NAME,  VALUE)

cord.wide
str(cord.wide)
sites <- c(unique(cord.wide$SITE_ID))
 
cord.wide <- cord.wide[c("longitude", "latitude")] #pull last two columns and reorder

#add number of sites and long/lat coords to data list:
dat$n.sites <- length(sites)
dat$longlat <- cord.wide

#make data spatially explicit
coordinates(cord.wide) = c("longitude", "latitude") 
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

proj4string(cord.wide) <- crs.geo  # define projection system of our data to WGS84
summary(cord.wide) 

#create a distance matrix between sites, best fit distance function TBD
distance.mat <- (distm(cord.wide, fun = distVincentyEllipsoid)/1000) #km distance
rownames(distance.mat) <- sites
colnames(distance.mat) <- sites

#add distance matrix and maximum distance between sites (km) to data list
dat$distance.mat <- distance.mat
dat$max.distance <- max(distance.mat)
summary(dat)
# ---------------------------------------------------------------------------------------------------
# ENVIRONMENTAL COVARIATES
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

#NOTE: IF THE DATA ARE COMPLETELY BALANCED OVER SPACE AND TIME ... the number of rows in "dat$comm.wide" should be the same as nrow in "dat$env" should be the same as no. of years * no. of sites
nrow(dat$comm.wide); nrow(dat$env); dat$n.years * dat$n.sites

# Are all year-by-site combinations in community data matched by environmental data?
ifelse(nrow(dat$comm.wide) == nrow(dat$env), "Yes", "No")

# Are community data balanced over space and time?
ifelse(nrow(dat$comm.wide) == dat$n.years * dat$n.sites, "Yes", "No")

# Inspect summary of 'dat' list 
summary(dat)

#clean up the workspace
rm("agg_c","agg_e","avgdata","comm.long","comm.wide","cord","cord.wide","crs.geo","dat.long", "data.key", "data.set","distance.mat","env.long", "env.wide","package","sites", "JRNdata", "JRNdata1", "n.obs", "envdata", "commdata", "data", "data1", "data2", "data3")
ls()

# Now, explore the data and perform further QA/QC by sourcing this script within the scripts "2_explore_spatial_dat.R", "3_explore_comm_dat.R", and "4_explore_environmental_dat.R"

# ---------------------------------------------------------------------------------------------------
## WRITE OUT DATA FOR ARCHIVING ##
#save flat files into 'final_data' folder on Google Drive. 

##### OLD WAY #####
#write .Rdata object into the "Intermediate_data" directory 
#filename <- paste(data.set,".Rdata", sep="")
#save(dat, file = paste("Intermediate_data/",filename,sep=""))



