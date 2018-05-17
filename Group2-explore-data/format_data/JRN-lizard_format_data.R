# --------------------------------------------------------- #
# Format raw data as a list of tables                       #
# JORNADA lizard data                                                      # Riley Andrade (main contact)
# Revised 09 Jun 2017                                       #
# --------------------------------------------------------- #

# Contributors: Riley Andrade, Max Castorani, Nina Lany, Sydne Record, Nicole Voelker

# Clear environment
rm(list = ls())

# Make sure your working environment is set to the GitHub repository ltermetacommunities. 

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

# JRN LTER (Jornada lizards from lter repository download in L0 directory; knb-lter-jrn.2100007001.13)
data.set <- "JRN-lizard"
data.key <- "0B7o8j0RLpcxiemtYVjF0ZGVxaVE" # Google Drive file ID

data <-  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", data.key), skip = 47, stringsAsFactors = F)

#Google Drive File Stream method:
data <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/JRN-lizards/JornadaStudy_007_npp_lizard_pitfall_trap_data.csv", skip = 47, stringsAsFactors=F)


#in this table, each row represents a lizard. Remove columns with data measured on indivudal lizards (i.e. svl, sex, toe number)
data <- data %>%
	select(date, zone, site, plot, spp,toe_num, pc)

data$datetime <- as.POSIXct(data$date, format = "%m/%d/%Y")
data$year <- as.numeric(format(data$datetime,"%Y"))
str(data)

############################
#re-do the data cleaning that Andrew Hope did (descibed in his metadata sheet)
#remove first and last years due to incomplete sampling
data <- subset(data, data$year > 1989 & data$year < 2006)

#remove the monthly sampling (1990 and 1991 only) so that only quarterly data remain and sampling effort is equal across years
#read in and merge on the data file Andrew Hope provided (that does not contain the toe_num needed to calculate abundance of unique individuals per species per site per year) 
Hope_data <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/JRN-lizards/JRN_Lizard_spp.csv", stringsAsFactors=F)

Hope_data <- Hope_data %>%
	mutate(datetime = as.POSIXct(Hope_data$date, format = "%m/%d/%y")) %>%
	select(datetime)

data <- merge(data, Hope_data, by = "datetime", all.x=F, all.y=T)

#Look at sampling:
ss <- function(x) {length(unique(x))}
tapply(data$site,data$year, ss) #two sites began in 1995
tapply(data$site,data$year, unique) #they were "NORT" and "SUMM"

#remove the two sites that began later, NORT and SUMM
data <- data %>%
	filter(site != "NORT" & site != "SUMM")

#Now remove the rows where no lizards were observed.
data <- data %>%
	filter(spp != "NONE")
	
	#NOTE: pc is a problem code. Look it up. Also, need species codes (http://jornada.nmsu.edu/data-catalogs/species/lter-plants) Are there uknkowns in there?


#assume equal sampling effort (Pitfall traps were opened for 2 week sessions  4x per year (quarterly) at each site; trapping began at sites XXXX and XXXX in 1996) and calculate the number of unique individuals of each species captured per year: 
length(unique(data$spp))  

#make expanded grid of all spp-plot-year combinations
species <- unique(data$spp)
sites <- unique(data$site)
years <- unique(data$year)
grid <- as.data.frame(expand.grid(species, sites, years))
names(grid) <- c("VARIABLE_NAME", "SITE_ID", "DATE")

#sum number of unique individuals counted per spp-site-year:
sum_dat <- data %>%
  group_by(year, site, spp) %>%
  summarize(count = ss(toe_num))%>%
  as.data.frame()

names(sum_dat) <- c("DATE", "SITE_ID", "VARIABLE_NAME","VALUE")
str(sum_dat)

#merge the two together, replace NA with 0, and name according to Erics format:
comm.long <- merge(sum_dat, grid, by = c("VARIABLE_NAME", "SITE_ID", "DATE"), all = T)
comm.long$OBSERVATION_TYPE = rep("TAXON_COUNT", length(comm.long$DATE))
comm.long$VARIABLE_UNITS = rep("count", length(comm.long$DATE))
comm.long$VALUE <- as.numeric(ifelse(is.na(comm.long$VALUE), 0, comm.long$VALUE))

comm.long$VALUE  <- ifelse((comm.long$SITE_ID == "NORT" |comm.long$SITE_ID == "SUMM") & comm.long$DATE < 1995, "NA", comm.long$VALUE)

comm.long$VALUE <- as.numeric(comm.long$VALUE)
comm.long <- na.omit(comm.long)
str(comm.long)

### COORDINATE DATA ### 
# read in coordinate data
dat.coord <- read.csv("~/Google Drive FIle Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/JRN-lizards/JRN_Lizard_meta.csv", stringsAsFactors=F, skip = 38) %>%
  tbl_df() %>%
  gather(VARIABLE_NAME, VALUE, Latitude:Longitude) %>%
  mutate(VARIABLE_NAME = tolower(VARIABLE_NAME),
         VARIABLE_UNITS = "dec degrees",
         OBSERVATION_TYPE = "SPATIAL_COORDINATE",
         SITE_ID = Site, 
         DATE = NA) %>%
  select(-Site) %>%
  select(-Zone.Site)
str(dat.coord)


### ENVIRONMENTAL DATA ###
dat.env <- read.csv("~/Google Drive FIle Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/JRN-lizards/JRN_Lizard_precip.csv", skip = 11, stringsAsFactors=F) %>%
  tbl_df() %>%
  mutate(VARIABLE_NAME = "precip",
         VARIABLE_UNITS = "mm",
         OBSERVATION_TYPE = "TAXON_COUNT",
         SITE_ID = paste(zone, site, sep = "-"), 
         DATE = date,
         OBSERVATION_TYPE = "ENV_VAR", 
         VALUE = est_precp) %>%
  select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE)


#rbind the L3 dataset
dat.tog <- rbind(dat.coord, comm.long, dat.env) %>%
  select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE)

#Write out the L3 dataset
write.csv(dat.tog, "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-jrn-lizards-hope.csv", row.names=F)


# --------------------------------------------------------- #
# Format raw data as a list of tables                       #
#                                                           #
# Revised 30 Mar 2017                                       #
# --------------------------------------------------------- #

# Contributors: Riley Andrade, Max Castorani, Nina Lany, Sydne Record, Nicole Voelker

# ---------------------------------------------------------------------------------------------------
#to match naming system

dat.long <- dat.tog


# MAKE DATA LIST
dat <- list()

# COMMUNITY DATA 
comm.long <- dat.long[dat.long$OBSERVATION_TYPE == "TAXON_COUNT", ] 
comm.long <- comm.long %>%
  droplevels()

str(comm.long)  # Inspect the structure of the community data

#Add number of unique taxa and number of years to data list:
dat$n.spp <- length(unique(comm.long$VARIABLE_NAME));dat$n.spp
dat$n.years <- length(unique(comm.long$DATE));dat$n.years

# Ensure that community data VALUE and DATE are coded as numeric
comm.long <- comm.long %>%   # Recode if necessary
  mutate_at(vars(c(DATE, VALUE)), as.numeric)

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

#Here's some code you could use if they are not:

# Ensure that community character columns coded as factors are re-coded as characters
#comm.long <- comm.long %>%   # Recode if necessary
#  mutate_if(is.factor, as.character)

# Ensure that SITE_ID is a character: recode numeric as character 
#comm.long <- comm.long %>%   # Recode if necessary
#  mutate_at(vars(SITE_ID), as.character)


# ---------------------------------------------------------------------------------------------------
# Check balanced sampling of species across space and time by inspecting table, and add to data list
xtabs(~ SITE_ID + DATE, data = comm.long)

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
## SUMMARIZE BY YEAR (OR SOMETHING) BEFORE RUNNING THIS!
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
rm("comm.long","comm.wide","cord","cord.wide","crs.geo","dat.long", "data.key", "data.set","distance.mat","env.long", "env.wide","package","sites", "ss", "data", "species", "sites", "years","Hope_data", "dat.coord", "dat.env", "dat.tog", "grid", "sum_dat", "test")
ls()

# Now, explore the data and perform further QA/QC by sourcing this script within the scripts "2_explore_spatial_dat.R", "3_explore_comm_dat.R", and "4_explore_environmental_dat.R"




