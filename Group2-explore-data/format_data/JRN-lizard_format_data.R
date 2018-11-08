# --------------------------------------------------------- #
# Format raw data as a list of tables                       #
# JORNADA lizard data                                                      # Riley Andrade (main contact)
# Revised 09 Jun 2017  & 04 Nov 2018 by Nina Lany                                     #
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
#data.set <- "JRN-lizard"
#data.key <- "0B7o8j0RLpcxiemtYVjF0ZGVxaVE" # Google Drive file ID

#data <-  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", data.key), skip = 47, stringsAsFactors = F)

#Google Drive File Stream method:
data <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/JRN-lizards/JornadaStudy_007_npp_lizard_pitfall_trap_data.csv", skip = 47, stringsAsFactors=F)

# direct download from LTER Data Portal
# Package ID: knb-lter-jrn.210007001.36 Cataloging System:https://pasta.edirepository.org.
# Data set title: Lizard pitfall trap data (LTER-II, LTER-III).
# Data set creator:  David Lightfoot -  
# Metadata Provider:    - Jornada Basin LTER 
# Contact:    - Data Manager Jornada Basin LTER  - datamanager@jornada-vmail.nmsu.edu
# Contact:  John Anderson -    - 
#data <- read.csv('https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-jrn.210007001.36&entityid=21cc9e0b7c5d27d1efd8d5e1db3ac4f1', skip = 47, stringsAsFactors = F)

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
	
#Now remove the rows where no lizards were observed.  Can't find apecies codes at (http://jornada.nmsu.edu/data-catalogs/species/lter-plants), but I'm guessing UKLI is 'unkown lizard' and UKCN is 'unknown ctenosaur'.

data <- data %>%
	filter(spp != "UKLI" & spp != "UKCN")
	

	#NOTE: pc is a problem code. Look it up. 

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
dat.coord <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/JRN-lizards/JRN_Lizard_meta.csv", stringsAsFactors=F, skip = 38) %>%
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
dat.env <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/JRN-lizards/JRN_Lizard_precip.csv", skip = 11, stringsAsFactors=F) %>%
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

