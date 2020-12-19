# Download CAP bird data

#################################################################################################
# Point-count bird censusing: long-term monitoring in central Arizona-Phoenix, ongoing since 2000
# RKA 05/15/2018                                    
#################################################################################################

rm(list = ls())
# Check for and install required packages
for (package in c('dplyr', 'tidyverse', 'tidyr')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, dependencies=TRUE, repos='http://cran.rstudio.com/')
    library(package, character.only=T)
  }
}

# Package ID: knb-lter-cap.46.15 Cataloging System:https://pasta.edirepository.org.
# Data set title: Point-count bird censusing: long-term monitoring of bird abundance and diversity in       central Arizona-Phoenix, ongoing since 2000.
# Data set creator:  Heather Bateman - Arizona State University, Polytechnic campus 
# Data set creator:  Dan Childers - Arizona State University 
# Data set creator:  Madhusudan Katti - Department of Forestry and Environmental Resources 
# Data set creator:  Eyal Shochat - Ben-Gurion University of the Negev 
# Data set creator:  Paige Warren - University of Massachusetts-Amherst 
# Metadata Provider:  Stevan Earl - Arizona State University 
# Contact:    - Data Manager Julie Ann Wrigley Global Institute of Sustainability, Arizona State University  - caplter.data@asu.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cap/46/15/1ac975d0d3272f2eabe68a66e9f908ad" 
infile1 <- sub("^https","http",infile1) 
dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "site_code",     
                 "location_type",     
                 "survey_date",     
                 "time_start",     
                 "time_end",     
                 "observer_name_part",     
                 "wind_speed",     
                 "wind_dir",     
                 "air_temp",     
                 "cloud_cover",     
                 "survey_notes",     
                 "human_activity_notes",     
                 "wind",     
                 "precipitation",     
                 "disturbances",     
                 "sight_obstruct",     
                 "noise_level",     
                 "site_condition",     
                 "non_bird_species",     
                 "code",     
                 "common_name",     
                 "distance",     
                 "bird_count",     
                 "observation_notes",     
                 "seen",     
                 "heard",     
                 "direction",     
                 "QCcomment"    ), check.names=TRUE, stringsAsFactors=F)

# Looking at the input data frame
str(dt1) 
head(dt1)
unique(dt1$site_code) #sites of point counts
unique(dt1$location_type) #classification of sites (checked for different sampling among classifications)                        

#Changing column format 
dt1$location_type <- as.character(dt1$location_type) #change location type to character to subset
dt1$site_code <- as.character(dt1$site_code) #change site code to character to subset
dt1$code <- as.character(dt1$code) #change species code to character to subset

#Extract year
dt1$Date <- as.POSIXct(dt1$survey_date)
dt1$Year <- as.numeric(format(dt1$Date, format = "%Y"))
dt1$Month <- as.numeric(format(dt1$Date, format = "%m"))
unique(dt1$Year) #number of years with observations (any number)
unique(dt1$Month)

#Check sampling within each year at each site classification
en <- function(x) {length(unique(x))} 
tapply(dt1$Date, list(dt1$location_type, dt1$Year), en) #SUBSET BY ESCA for 2000-onward dataset

#                     2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017
#DesertFertilization   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA    4   22
#ESCA                  45  141  135  110  111   82   60   66   60   64   71   70   67   79   75   76   72   47
#NDV                   NA   NA   NA    2   13    9    8    6    7    7    7    5    6    7   10    7    6   NA
#PASS                  NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA    8   42
#Riparian              20   88   67   21   82   64   46   42   44   42   49   50   47   49   50   53   44   NA
#SRBP                  NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   19   44   45   36   37

#Remove location types without consistent long-term sampling effort
dat <- dt1 %>% filter(location_type == "ESCA", #selecting subset of ESCA bird data (even sampling and design for 2001-onward)
                      Year != 2017,  #Drop 2017 until new data is updated in the file (2017 and 2018 data)
                      site_code != "M-9",
                      site_code != "V-18",
                      site_code != "X-8",
                      site_code != "V-16", #Drop sites M9, V18, V16, and X8 for uneven sampling design
                      ! grepl('Unidentified', common_name), #Remove unidentified species (~2% of total observations)
                      ! distance  %in% c(">40","FT" ), #Observations within 40m
                      ! is.na(distance), #Observations within 40m
                      Month == 3 | Month == 4 | Month == 5)  #Drop all but spring months for uneven sampling design

#check sampling within each year at each plot
tapply(dat$Date, list(dat$site_code, dat$Year), en) 
tapply(dat$site_code, dat$Year, en) 

#select only points with complete cases
cc <- tapply(dat$Date, list(dat$site_code, dat$Year), en)
cc <- as.data.frame(cc)
cc$site_code <- row.names(cc)
cc$Code <- complete.cases(cc)
XX <- subset(cc, Code == "TRUE")
XX$Plot
data <- merge(dat, XX, by = "site_code", all.x = F, all.y=T)
data <- data %>% select(site_code,location_type,Date, Year,Month, distance,code,bird_count)

#calculate the maximum number of each species observed at each point in each year
out <- data.frame(OBSERVATION_TYPE = "", 
                  SITE_ID = "",
                  DATE = "",
                  VARIABLE_NAME = "",
                  VARIABLE_UNITS = "",
                  VALUE = "")

step1 <- data %>% group_by(Date, site_code, code) %>% 
  summarize(NUM = sum(bird_count))
step1$Year<- as.numeric(format(step1$Date, format = "%Y"))

step2 <- step1 %>% group_by(Year, site_code, code) %>% 
  summarize(VALUE = max(NUM))

out <- cbind.data.frame(OBSERVATION_TYPE = "TAXON_COUNT",
                        SITE_ID = step2$site_code,
                        DATE = step2$Year,
                        VARIABLE_NAME = step2$code,
                        VARIABLE_UNITS = "count",
                        VALUE = step2$VALUE)

# Convert from long to wide and back to long to be sure that we have fully propagated taxa
out_wide <- spread(out, key = VARIABLE_NAME, value = VALUE, fill = 0)
out_long <- gather(out_wide, key = VARIABLE_NAME, value = VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS)

#check sampling within each year at each plot
tapply(out_long$VARIABLE_NAME, list(out_long$SITE_ID, out_long$DATE), en) 
tapply(out_long$SITE_ID, out_long$DATE, en) 

# Write CSV file for cleaned data (L3)
write.csv(out_long, file = "manuscripts/ms1/L3-cap-birds-banville.csv", row.names = F)

############ Download Santa Barbara Coastal data ####################################
# --------------------------------------------------------- #
# Format raw data as a list of tables                       #
# separate by taxa to make four separate L3 datasets                                  #
# Revised Nov 17, 2018 by MCN Castorani                     #
# --------------------------------------------------------- #

# Contributors: Riley Andrade, Max Castorani, Nina Lany, Sydne Record, Nicole Voelker

# Import community data from EDI

# Package ID: knb-lter-sbc.50.6 Cataloging System:https://pasta.edirepository.org.
# Data set title: SBC LTER: Reef: Annual time series of biomass for kelp forest species, ongoing since 2000.
# Data set creator:    - Santa Barbara Coastal LTER 
# Data set creator:  Daniel C Reed -  
# Contact:    - Information Manager, Santa Barbara Coastal LTER   - sbclter@msi.ucsb.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/50/7/24d18d9ebe4f6e8b94e222840096963c" 

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/50/7/24d18d9ebe4f6e8b94e222840096963c" 


infile1 <- sub("^https","http",infile1) 
dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "YEAR",     
                 "MONTH",     
                 "DATE",     
                 "SITE",     
                 "TRANSECT",     
                 "SP_CODE",     
                 "PERCENT_COVER",     
                 "DENSITY",     
                 "WM_GM2",     
                 "DM_GM2",     
                 "SFDM",     
                 "AFDM",     
                 "SCIENTIFIC_NAME",     
                 "COMMON_NAME",     
                 "TAXON_KINGDOM",     
                 "TAXON_PHYLUM",     
                 "TAXON_CLASS",     
                 "TAXON_ORDER",     
                 "TAXON_FAMILY",     
                 "TAXON_GENUS",     
                 "GROUP",     
                 "MOBILITY",     
                 "GROWTH_MORPH",
                 "COARSE_GROUPING"), check.names=TRUE, stringsAsFactors=F)


# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DATE) == length(tmp1DATE[!is.na(tmp1DATE)])){dt1$DATE <- tmp1DATE } else {print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DATE)


comm.dat.unformatted <- dt1; rm(dt1)

# Recode "-99999" in unformatted community data
comm.dat.unformatted[comm.dat.unformatted == -99999] <- NA 
summary(comm.dat.unformatted)
#which taxa have NA values in which years? 
how.many.na <- function(x) {length(which(is.na(x)))}
Tbl <- tapply(comm.dat.unformatted$DM_GM2, list(comm.dat.unformatted$SCIENTIFIC_NAME, comm.dat.unformatted$YEAR), how.many.na)
#problem of many NA values in early years for some taxa that was present in knb.sbc.50.6 now appears to be fixed. A few NA in a few years only.

# ---------------------------------------------------------------------------------------------------
# Reformat community data
comm.dat.long <- comm.dat.unformatted %>%
  dplyr::select(YEAR, SITE, SP_CODE, DM_GM2, GROUP, MOBILITY) %>%  # Select variables of interest
  dplyr::filter(SITE != "SCTW") %>%  # Remove two Channel Islands sites
  dplyr::filter(SITE != "SCDI") %>%
  droplevels() %>%
  mutate(VARIABLE_NAME = SP_CODE,   # Reformat column names
         OBSERVATION_TYPE = "TAXON_COUNT",
         VALUE = DM_GM2,
         VARIABLE_UNITS = "g dry per m2",
         DATE = YEAR,
         SITE_ID = SITE,
         TAXON_GROUP = paste(MOBILITY, GROUP, sep = " ")) %>%
  dplyr::select(-DM_GM2, -SP_CODE, -YEAR, -SITE, -GROUP) %>%
  
  # These data are still at transect-level. Group by year, site, and taxon to calculate mean biomass
  dplyr::group_by(SITE_ID, DATE, VARIABLE_NAME) %>%
  dplyr::summarise(VALUE = mean(VALUE, na.omit = TRUE),
                   OBSERVATION_TYPE = unique(OBSERVATION_TYPE),
                   VARIABLE_UNITS = unique(VARIABLE_UNITS),
                   TAXON_GROUP = unique(TAXON_GROUP)) %>%
  ungroup() %>%
  dplyr::select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE, TAXON_GROUP)


comm.dat.long$TAXON_GROUP <- dplyr::recode(comm.dat.long$TAXON_GROUP, 
                                           `MOBILE FISH`  = "FISH",
                                           `SESSILE ALGAE` = "ALGAE")

comm.dat.long <- comm.dat.long[, c("OBSERVATION_TYPE", "SITE_ID", "DATE", "VARIABLE_NAME", "VARIABLE_UNITS", "VALUE", "TAXON_GROUP")] # Reorder columns

# # Spread from long to wide and then gather back to long to ensure equal observations across sites and time
# comm.dat.wide <- comm.dat.long %>%
#   dplyr::select(-TAXON_GROUP) %>%
#   spread(key = VARIABLE_NAME,  value = VALUE, fill = 0)

rm(comm.dat.unformatted)

# ---------------------------------------------------------------------------------------------------
# REMOVE YEAR 2000 BECAUSE WE ARE MISSING DATA
comm.dat.long <- comm.dat.long[comm.dat.long$DATE > 2000, ] %>%
  drop_na()  # Remove NA rows

# ---------------------------------------------------------------------------------------------------
# Put datasets together by group
unique(comm.dat.long$TAXON_GROUP)

# ALGAE (INCLUDING GIANT KELP)
sbc.algae.temp <- comm.dat.long %>%
  dplyr::filter(TAXON_GROUP == "ALGAE") %>%
  dplyr::select(-TAXON_GROUP)

sbc.algae.w.kelp <- rbind(sbc.xy, sbc.env, sbc.algae.temp); rm(sbc.algae.temp)       # Algae including giant kelp
sbc.algae.no.kelp <- sbc.algae.w.kelp[!sbc.algae.w.kelp$VARIABLE_NAME == "MAPY", ]   # Algae excluding giant kelp

# SESSILE INVERTEBRATES
sbc.sessile.temp <- comm.dat.long %>%
  dplyr::filter(TAXON_GROUP == "SESSILE INVERT") %>%
  dplyr::select(-TAXON_GROUP)

sbc.sessile <- rbind(sbc.xy, sbc.env, sbc.sessile.temp); rm(sbc.sessile.temp)

# MOBILE INVERTEBRATES
sbc.mobile.temp <- comm.dat.long %>%
  dplyr::filter(TAXON_GROUP == "MOBILE INVERT") %>%
  dplyr::select(-TAXON_GROUP)

sbc.mobile <- rbind(sbc.xy, sbc.env, sbc.mobile.temp); rm(sbc.mobile.temp)

# FISHES
sbc.fish.temp <- comm.dat.long %>%
  dplyr::filter(TAXON_GROUP == "FISH") %>%
  dplyr::select(-TAXON_GROUP)

sbc.fish <- rbind(sbc.xy, sbc.env, sbc.fish.temp); rm(sbc.fish.temp)

# Write CSV file for the cleaned (L3) sessileInverts dataset for MS1:

write.csv(sbc.sessile, file = "manuscripts/ms1/L3-sbc-sessileInverts-castorani.csv", row.names = F)

###################### Download Mt. St. Helens Data ########################
# read data online
spp_abundance <- read.csv('http://esapubs.org/archive/ecol/E091/152/MSH_SPECIES_PLOT_YEAR.csv', stringsAsFactors=F) %>%
  gather(species, cover, Abilas:Xerten) %>%
  setNames( tolower(names(.)) )
plot_d <- read.csv('http://esapubs.org/archive/ecol/E091/152/MSH_PLOT_DESCRIPTORS.csv', fileEncoding = "ISO-8859-1") %>%
  setNames( tolower(names(.)) )
taxa_d  <- read.csv('http://esapubs.org/archive/ecol/E091/152/MSH_SPECIES_DESCRIPTORS.csv', fileEncoding = "ISO-8859-1")

# Format data ----------------------------------------------------------------------

# Remove non-existing data from plot table
plot_info <- subset(plot_d ,plot_code !="")

# Create "plot code" in spp_abundance data frame
plot_num  <- spp_abundance$plot_number
plot_n_ch <- as.character(plot_num)
plot_n_ch[which(plot_num < 10)] <- paste0("0",
                                          plot_n_ch[which(plot_num < 10)])
plot_code_vec <- paste0(spp_abundance$plot_name, plot_n_ch)
spp_abundance <- mutate(spp_abundance, plot_code = plot_code_vec)


# Prepare three "observation type" tables 

# Spatial coordinates --------------------------------------------------
plot_long   <- format_data(plot_info,
                           OBSERVATION_TYPE = "ENV_VAR", 
                           SITE_ID = "plot_code", 
                           DATE = NA, # NA because non-temporal
                           VARIABLE_NAME = "longitude", 
                           VARIABLE_UNITS = "dec.degrees",  
                           VALUE = "long...")

plot_lat   <- format_data(plot_info,
                          OBSERVATION_TYPE = "ENV_VAR", 
                          SITE_ID = "plot_code", 
                          DATE = NA, # NA because non-temporal
                          VARIABLE_NAME = "latitude", 
                          VARIABLE_UNITS = "dec.degrees",  
                          VALUE = "lat...")

# Stack longitude and latitude together
plot_coord  <- rbind(plot_long,plot_lat)


# Species abundance data -------------------------------------------
spp_abundance <- rename(spp_abundance,VALUE = cover,
                        SITE_ID = plot_code,
                        VARIABLE_NAME = species,
                        DATE = year)
spp_abundance <- mutate(spp_abundance, 
                        OBSERVATION_TYPE = "TAXON_COUNT",
                        VARIABLE_UNITS = "PERCENT_COVER")
spp_abundance <- order_col(spp_abundance)


# Environmental data -----------------------------------------------

# heat load (could not find unit of measure. 
# Index is from McCune and Keon 2002 in JVS)
heat_load   <- format_data(plot_info,
                           OBSERVATION_TYPE = "ENV_VAR", 
                           SITE_ID = "plot_code", 
                           DATE = NA, # NA because non-temporal
                           VARIABLE_NAME = "heat_load", 
                           VARIABLE_UNITS = "Adimensional(0 to 1)",  
                           VALUE = "heat_load")

# Elevation
elevation_m   <- format_data(plot_info,
                             OBSERVATION_TYPE = "ENV_VAR", 
                             SITE_ID = "plot_code", 
                             DATE = NA, # NA because non-temporal
                             VARIABLE_NAME = "elevation", 
                             VARIABLE_UNITS = "meter",  
                             VALUE = "elevation.m.")

# Aspect (according to Arabic compass rose)
aspect   <- format_data(plot_info,
                        OBSERVATION_TYPE = "ENV_VAR", 
                        SITE_ID = "plot_code", 
                        DATE = NA, # NA because non-temporal
                        VARIABLE_NAME = "aspect", 
                        VARIABLE_UNITS = "Arabic_compass_rose",  
                        VALUE = "aspect")

# Slope in decimal degrees
slope   <- format_data(plot_info,
                       OBSERVATION_TYPE = "ENV_VAR", 
                       SITE_ID = "plot_code", 
                       DATE = NA, # NA because non-temporal
                       VARIABLE_NAME = "slope", 
                       VARIABLE_UNITS = "dec.degrees",  
                       VALUE = "slope...")

# Succession type (e.g. primary if all plant life removed,
# secondary if a few survivors were initially present
# disturbance in places where most species survived)
succession <-  format_data(plot_info,
                           OBSERVATION_TYPE = "ENV_VAR", 
                           SITE_ID = "plot_code", 
                           DATE = NA, # NA because non-temporal
                           VARIABLE_NAME = "succession_type", 
                           VARIABLE_UNITS = "categorical",  
                           VALUE = "succession_type")

# Type of disturbance
impact_type <-  format_data(plot_info,
                            OBSERVATION_TYPE = "ENV_VAR", 
                            SITE_ID = "plot_code", 
                            DATE = NA, # NA because non-temporal
                            VARIABLE_NAME = "impact_type", 
                            VARIABLE_UNITS = "categorical",  
                            VALUE = "impact_type")

# Stack these four data types on top of each other
env_vars <- rbind(heat_load, elevation_m, aspect, slope, 
                  succession, impact_type)

# Output file =========================================================
delmoral_data <- rbind(plot_coord,spp_abundance,env_vars) %>% 
  # select Abraham Plain sites located on PUMICE. 
  # This is the longest swath of continuous data
  subset( grepl('PUPL', SITE_ID) )

write.csv(delmoral_data, "manuscripts/ms1/L3-and-plants-mtStHelens.csv", row.names = F)

#MS1 Box 1 Figures: species accumulation curve (richness and time to asymptote) depends on number of sites sampled.
#NKL 06/27/2018


# Check for and install required packages
for (package in c('dplyr', 'tidyverse', 'vegan', 'googledrive')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


# ---------------------------------------------------------------------------------------------------
# SPECIES ACCUMULATION CURVE FUNCTION - OVER TIME

# Make a function that returns the cumulative number of taxa observed for a given set of community data
cuml.taxa.fun <- function(EX){
  taxa.t.list <- list() # Make empty list
  dates <- unique(EX$DATE)
  # Loop over each year, creating a list that contains the unique taxa found in each year
  for(t in 1:length(unique(EX$DATE))){
    tmp.dat <- subset(EX, EX$DATE == dates[t])
    tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0) 
    taxa.t.list[[t]] <- unique(tmp.dat.pres$VARIABLE_NAME)
  }
  
  # Loop over each year, creating a list that contains the unique taxa found in each year
  #  for(t in 1:length(unique(EX$DATE))){
  #    tmp.dat <- subset(EX, EX$DATE == t + (min(EX$DATE) - 1))
  #    tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0) 
  #    taxa.t.list[[t]] <- unique(tmp.dat.pres$VARIABLE_NAME)
  #  }
  
  # Make cumulative list of taxa through time
  cuml.taxa <- list() # Empty list
  cuml.taxa[[1]] <- taxa.t.list[[1]] # Add the taxa from the first time step 
  
  # Run for-loop to create list of the cumulative taxa, with duplicates
  for(t in 2:length(unique(EX$DATE))){ 
    cuml.taxa[[t]] <- c(cuml.taxa[[t - 1]], taxa.t.list[[t]])
  }
  
  # Remove duplicates
  cuml.taxa <- lapply(cuml.taxa, function(x){unique(x)})
  
  # Return the number of total unique taxa through time
  cuml.no.taxa <- data.frame("year" = unique(EX$DATE))
  cuml.no.taxa$no.taxa <- unlist(lapply(cuml.taxa, function(x){length(x)}))
  
  return(cuml.no.taxa)
}

#---------------------------------------------------------------------------------------------------
# SPECIES ACCUMULATION CURVE FUNCTION - OVER SPACE

# Make a function that returns the cumulative number of taxa observed for a given set of community data
cuml.taxa.space.fun <- function(EX){
  taxa.s.list <- list() # Make empty list
  sites <- unique(EX$SITE_ID)
  # Loop over each year, creating a list that contains the unique taxa found in each year
  for(t in 1:length(unique(EX$SITE_ID))){
    tmp.dat <- subset(EX, EX$SITE_ID == sites[t])
    tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0) 
    taxa.s.list[[t]] <- unique(tmp.dat.pres$VARIABLE_NAME)
  }
  
  # Make cumulative list of taxa over space
  cuml.taxa.space <- list() # Empty list
  cuml.taxa.space[[1]] <- taxa.s.list[[1]] # Add the taxa from the first sites 
  
  # Run for-loop to create list of the cumulative taxa, with duplicates
  for(t in 2:length(unique(EX$SITE_ID))){ 
    cuml.taxa.space[[t]] <- c(cuml.taxa.space[[t - 1]], taxa.s.list[[t]])
  }
  
  # Remove duplicates
  cuml.taxa.space <- lapply(cuml.taxa.space, function(x){unique(x)})
  
  # Return the number of total unique taxa over space
  cuml.no.taxa.space <- data.frame("site" = unique(EX$SITE_ID))
  cuml.no.taxa.space$no.taxa <- unlist(lapply(cuml.taxa.space, function(x){length(x)}))
  
  return(cuml.no.taxa.space)
}

# Box 1, Figure 1a-b: cap-birds-banville
L3dat <- read.csv("L3-cap-birds-banville.csv", header = T, stringsAsFactors=F)

# MAKE DATA LIST
dat <- list()

# Subset out community data and add it to dat list
dat$comm.long <- subset(L3dat, OBSERVATION_TYPE=='TAXON_COUNT')

# Ensure that community data VALUE and DATE are coded as numeric
dat$comm.long <- dat$comm.long %>%   # Recode if necessary
  mutate_at(vars(c(DATE, VALUE)), as.numeric)

# Ensure that SITE_ID is a character: recode numeric as character 
dat$comm.long <- dat$comm.long %>%   # Recode if necessary
  mutate_at(vars(SITE_ID), as.character)

# Double-check that all columns are coded properly
ifelse(FALSE %in% 
         c(
           class(dat$comm.long$OBSERVATION_TYPE) == "character",
           class(dat$comm.long$SITE_ID) == "character",
           class(dat$comm.long$DATE) == "numeric",
           class(dat$comm.long$VARIABLE_NAME) == "character",
           class(dat$comm.long$VARIABLE_UNITS) == "character",
           class(dat$comm.long$VALUE) == "numeric"
         ),
       "ERROR: Community columns incorrectly coded.", 
       "OK: Community columns correctly coded.")
# 


# Convert community data to wide form
comm.wide <- dat$comm.long %>%
  select(-VARIABLE_UNITS) %>%
  spread(VARIABLE_NAME,  VALUE)

dat$comm.wide <- comm.wide
summary(dat)


#make subsets of data:
sites <- unique(dat$comm.long$SITE_ID)
set.seed(12)
sites <- sample(sites,length(sites), replace=F)

one.site <- subset(dat$comm.long, SITE_ID==sites[1])
five.sites <- subset(dat$comm.long, SITE_ID %in% sites[1:5])
twenty.sites <- subset(dat$comm.long, SITE_ID %in% sites[1:20])

# Calculate cumulative taxa (i.e., species accumulation) across all sites pooled together
cum.taxa.all.sites <- cuml.taxa.fun(EX = dat$comm.long)
cum.taxa.one.site <- cuml.taxa.fun(EX = one.site)
cum.taxa.five.sites <- cuml.taxa.fun(EX = five.sites)
cum.taxa.twenty.sites <- cuml.taxa.fun(EX = twenty.sites)

####
##### TEST DIFFERENT SPP ACCUM MODELS ###########
#with vegan, fit species accumulation curve:
year <- cum.taxa.all.sites$year - min(cum.taxa.all.sites$year) +1
xtmp <- seq(min(year), max(year), len=51)
## all sites:
S <- cum.taxa.all.sites$no.taxa

##The Arrhenius model:(SSarrhenius) is the expression k*area^z. This is the most classical model that can be found in any textbook of ecology (and also in Dengler 2009). Parameter z is the steepness of the species-area curve, and k is the expected number of species in a unit area.
marr <- nls( S ~SSarrhenius(year, k, z))
confint(marr) #z = steepness and k = expected number of species

# This plot was used for exploratory data analysis of the different models, so it is commented out.
#plot(S ~ year, xlab = "Time (years)", ylab = "Number of Species", ylim = c(1, max(S)))
#lines(xtmp, predict(marr, newdata=data.frame(year = xtmp)), lwd=2)

## Lomolino: using original names of the parameters (Lomolino 2000):
#The Lomolino model (SSlomolino) is Asym/(1 + slope^log(xmid/area)) (Lomolino 2000, Dengler 2009). Parameter Asym is the asymptotic maximum number of species, slope is the maximum slope of increase of richness, and xmid is the area where half of the maximum richness is achieved.
mlom <- nls(S ~ SSlomolino(year, Smax, A50, Hill))
mlom; confint(mlom)
#lines(xtmp, predict(mlom, newdata=data.frame(year=xtmp)), lwd=2, col = 4)

## Michaelis Menten:
mmic <- nls(S ~ SSmicmen(year, slope, Asym))
#lines(xtmp, predict(mmic, newdata = data.frame(year=xtmp)),lwd =2, col = 5)
mmic; confint(mmic)

## compare models (AIC)
allmods <- list(Arrhenius = marr,  Lomolino = mlom, MicMen= mmic)
sapply(allmods, AIC)
pred_all <- predict(mlom, newdata = data.frame(year=xtmp))


##### FIT THE REST  OF THE  YEARS  ###########
S_1 <- cum.taxa.one.site$no.taxa
S_5 <- cum.taxa.five.sites$no.taxa
S_20 <- cum.taxa.twenty.sites$no.taxa

#Commented out exploratory data analysis for looking at fits
#plot(year, S, pch = 19, ylim = c(0,max(cum.taxa.all.sites$no.taxa)+1), ylab = "Cumulative species richness", xlab = "Time (years)", bty="l", xaxt="n", main = "Sessile inverts at SBC LTER")
#lines(xtmp, pred_all, lwd=2)

#20 sites
#points(year, S_20, pch = 19, col=2)
mlom_20 <- nls(S_20 ~ SSlomolino(year, Smax, A50, Hill))
mmic_20 <- nls(S_20 ~ SSmicmen(year, slope, Asym))
marr_20 <- nls(S_20 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_20,  Lomolino = mlom_20, MicMen= mmic_20)
sapply(allmods, AIC)
pred_20 <- predict(mlom_20, newdata = data.frame(year=xtmp))
mlom_20; confint(mlom_20)
#lines(xtmp, predict(mlom_5, newdata=data.frame(year=xtmp)), lwd=2, col = 2)

#5 sites
#points(year, S_5, pch = 19, col=3)
cntrl <- nls.control(maxiter=300)
mlom_5 <- nls(S_5 ~ SSlomolino(year, Smax, A50, Hill), control = cntrl)
mmic_5 <- nls(S_5 ~ SSmicmen(year, slope, Asym))
marr_5 <- nls(S_5 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_5,  Lomolino = mlom_5, MicMen= mmic_5)
sapply(allmods, AIC)
pred_5 <- predict(marr_5, newdata = data.frame(year=xtmp))
marr_5; confint(marr_5)
#lines(xtmp, predict(mlom_5, newdata=data.frame(year=xtmp)), lwd=2, col = 3)

#1 site
#points(year, S_1, pch = 19, col=4)
mlom_1 <- nls(S_1 ~ SSlomolino(year, Smax, A50, Hill)) #error
mmic_1 <- nls(S_1 ~ SSmicmen(year, slope, Asym))
marr_1 <- nls(S_1 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_1, Lomolino = mlom_1, MicMen= mmic_1)
sapply(allmods, AIC)
pred_1 <- predict(mlom_1, newdata = data.frame(year=xtmp))
mlom_1; confint(mlom_1)
#lines(xtmp, predict(marr_1, newdata=data.frame(year=xtmp)), lwd=2, col = 4)

# Start making figure 1a for Box 1
pdf(file = "Box_1_Fig1.pdf", height = 7, width = 7)
colours <- c("coral4", "coral3", "coral2", "coral1")
par(mfrow = c(2,3))
par(mar = c(4, 6, 4, 1) + 0.1)
plot(year, S, pch = 19, ylim = c(0,max(cum.taxa.all.sites$no.taxa)+1), ylab = "Cumulative species richness", xlab = "", xaxt = "n", bty="l",  main = "", xlim = c(0,16), col = "coral4", cex=1.75, cex.axis=1.75, cex.lab=1.75)
lines(xtmp, pred_all, lwd=2, col = "coral4")
points(year, S_20, col="coral3", pch=19, cex=1.75)
lines(xtmp, pred_20, col="coral3")
points(year, S_5, col="coral2", pch=19, cex=1.75)
lines(xtmp, pred_5, col="coral2")
points(year, S_1, col="coral1", pch=19, cex=1.75)
lines(xtmp, pred_1, col="coral1")
axis(1, at = c (0,5,10,15), cex.axis=1.75)
text(1, 105, "A", cex=2, font = 2)
#plot legend
par(mar = c(4, 0, 4, 0) + 0.1)
plot.new()
legend("left", fill = colours, legend = c("35 sites", "20 sites","5 sites","1 site"), cex = 1.75, bty='n')

#note the graphics (Quartz) window is still open

## accumulation over space
no.taxa.space <- cuml.taxa.space.fun(dat$comm.long)
num.tax <- no.taxa.space$no.taxa
site <- seq(1, length(unique(dat$comm.long$SITE_ID)),1)
mlom <- nls(num.tax ~ SSlomolino(site, Smax, A50, Hill)) 
mmic <- nls(num.tax ~ SSmicmen(site, slope, Asym))
marr <- nls(num.tax ~ SSarrhenius(site, k, z))

allmods <- list(Arrhenius = marr, Lomolino = mlom, MicMen= mmic)
sapply(allmods, AIC)

# generate plot for Box 1 Figure 1b
par(mar = c(4, 0, 4, 2) + 0.1)
plot(rownames(no.taxa.space), no.taxa.space$no.taxa, pch = 19,  xaxt="n", bty="l", xlab = "", ylab = "", ylim = c(0,max(no.taxa.space$no.taxa)+1), col = "coral4", cex=1.75, cex.lab=1.75, cex.axis=1.75) #type = "o",
lines(site, predict(mmic, newdata=data.frame(site=site)), lwd=2, col = "coral4")
axis(side=1, at = rownames(no.taxa.space), labels = seq(1,length(no.taxa.space$site),1), cex.axis=1.75)

text(3, 105, "B", cex=2, font = 2)

# ---------------------------------------------------------------------------------------------------
# Box 1, Figures 1c-d sbc-sessileInverts-castorani
L3dat <- read.csv("L3-sbc-sessileInverts-castorani.csv", header = T, stringsAsFactors=F)

# MAKE DATA LIST
dat <- list()

# Subset out community data and add it to dat list
dat$comm.long <- subset(L3dat, OBSERVATION_TYPE=='TAXON_COUNT')

# Ensure that community data VALUE and DATE are coded as numeric
dat$comm.long <- dat$comm.long %>%   # Recode if necessary
  mutate_at(vars(c(DATE, VALUE)), as.numeric)

# Ensure that SITE_ID is a character: recode numeric as character 
dat$comm.long <- dat$comm.long %>%   # Recode if necessary
  mutate_at(vars(SITE_ID), as.character)

# Double-check that all columns are coded properly
ifelse(FALSE %in% 
         c(
           class(dat$comm.long$OBSERVATION_TYPE) == "character",
           class(dat$comm.long$SITE_ID) == "character",
           class(dat$comm.long$DATE) == "numeric",
           class(dat$comm.long$VARIABLE_NAME) == "character",
           class(dat$comm.long$VARIABLE_UNITS) == "character",
           class(dat$comm.long$VALUE) == "numeric"
         ),
       "ERROR: Community columns incorrectly coded.", 
       "OK: Community columns correctly coded.")

#Here, remove the first few years:
dat$comm.long <- subset(dat$comm.long, DATE > 2003)


#make subsets of data:
sites <- unique(dat$comm.long$SITE_ID)
set.seed(12)
sites <- sample(sites,length(sites), replace=F)

one.site <- subset(dat$comm.long, SITE_ID==sites[1])
two.sites <- subset(dat$comm.long, SITE_ID %in% sites[1:2])
five.sites <- subset(dat$comm.long, SITE_ID %in% sites[1:5])
eleven.sites <- subset(dat$comm.long, SITE_ID %in% sites[1:11])

# Calculate cumulative taxa (i.e., species accumulation) across all sites pooled together
cum.taxa.all.sites <- cuml.taxa.fun(EX = dat$comm.long)
cum.taxa.one.site <- cuml.taxa.fun(EX = one.site)
cum.taxa.two.sites <- cuml.taxa.fun(EX = two.sites)
cum.taxa.five.sites <- cuml.taxa.fun(EX = five.sites)

# Exploratory data analysis that is commented out
#plot(cum.taxa.all.sites$year, cum.taxa.all.sites$no.taxa, type = "o", pch = 19, ylim = c(0,max(cum.taxa.all.sites$no.taxa)+1), ylab = "Cumulative species richness", xlab = "Time (years)", bty="l", xaxt="n", main = "Sessile inverts at SBC LTER")
#points(cum.taxa.five.sites$year, cum.taxa.five.sites$no.taxa, type = "o", pch = 19, lty=2)
#points(cum.taxa.two.sites$year, cum.taxa.two.sites$no.taxa, type = "o", pch = 19, lty=3)
#points(cum.taxa.one.site$year, cum.taxa.one.site$no.taxa, type = "o", pch = 19, lty=4)
#legend("bottomright", lty = c(1,2,3,4), legend = c("11 sites", "five sites","two sites","one site"))
#axis(side = 1, at = c( 2004, 2009, 2014), labels = c(1,5,10))

####
##### TEST DIFFERENT SPP ACCUM MODELS ###########
#with vegan, fit species accumulation curve:
year <- cum.taxa.all.sites$year - min(cum.taxa.all.sites$year) +1
xtmp <- seq(min(year), max(year), len=51)
## all sites:
S <- cum.taxa.all.sites$no.taxa

##The Arrhenius model:(SSarrhenius) is the expression k*area^z. This is the most classical model that can be found in any textbook of ecology (and also in Dengler 2009). Parameter z is the steepness of the species-area curve, and k is the expected number of species in a unit area.
marr <- nls( S ~SSarrhenius(year, k, z))
confint(marr) #z = steepness and k = expected number of species

#Commented out exploratory data analysis
#plot(S ~ year, xlab = "Time (years)", ylab = "Number of Species", ylim = c(1, max(S)))
#lines(xtmp, predict(marr, newdata=data.frame(year = xtmp)), lwd=2)

## Lomolino: using original names of the parameters (Lomolino 2000):
#The Lomolino model (SSlomolino) is Asym/(1 + slope^log(xmid/area)) (Lomolino 2000, Dengler 2009). Parameter Asym is the asymptotic maximum number of species, slope is the maximum slope of increase of richness, and xmid is the area where half of the maximum richness is achieved.
mlom <- nls(S ~ SSlomolino(year, Smax, A50, Hill))
mlom; confint(mlom)
#lines(xtmp, predict(mlom, newdata=data.frame(year=xtmp)), lwd=2, col = 4)

## Michaelis Menten:
mmic <- nls(S ~ SSmicmen(year, slope, Asym))
#lines(xtmp, predict(mmic, newdata = data.frame(year=xtmp)),lwd =2, col = 5)
mmic; confint(mmic)

## compare models (AIC)
allmods <- list(Arrhenius = marr,  Lomolino = mlom, MicMen= mmic)
sapply(allmods, AIC)
pred_all <- predict(mlom, newdata = data.frame(year=xtmp))


##### FIT THE REST  OF THE  YEARS  ###########
S_1 <- cum.taxa.one.site$no.taxa
S_2 <- cum.taxa.two.sites$no.taxa
S_5 <- cum.taxa.five.sites$no.taxa

#for looking at fits commented out exploratory data analysis
#plot(year, S, pch = 19, ylim = c(0,max(cum.taxa.all.sites$no.taxa)+1), ylab = "Cumulative species richness", xlab = "Time (years)", bty="l", xaxt="n", main = "Sessile inverts at SBC LTER")
#lines(xtmp, pred_all, lwd=2)


#5 sites
#points(year, S_5, pch = 19, col=2)
mlom_5 <- nls(S_5 ~ SSlomolino(year, Smax, A50, Hill))
mmic_5 <- nls(S_5 ~ SSmicmen(year, slope, Asym))
marr_5 <- nls(S_5 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_5,  Lomolino = mlom_5, MicMen= mmic_5)
sapply(allmods, AIC)
pred_5 <- predict(mlom_5, newdata = data.frame(year=xtmp))
mlom_5; confint(mlom_5)
#lines(xtmp, predict(mlom_5, newdata=data.frame(year=xtmp)), lwd=2, col = 2)


#2 sites
#points(year, S_2, pch = 19, col=3)
mlom_2 <- nls(S_2 ~ SSlomolino(year, Smax, A50, Hill))
mmic_2 <- nls(S_2 ~ SSmicmen(year, slope, Asym))
marr_2 <- nls(S_2 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_2,  Lomolino = mlom_2, MicMen= mmic_2)
sapply(allmods, AIC)
pred_2 <- predict(mlom_2, newdata = data.frame(year=xtmp))
mlom_2; confint(mlom_2)
#lines(xtmp, predict(mlom_2, newdata=data.frame(year=xtmp)), lwd=2, col = 3)


#1 site
#points(year, S_1, pch = 19, col=4)
mlom_1 <- nls(S_1 ~ SSlomolino(year, Smax, A50, Hill)) #error
mmic_1 <- nls(S_1 ~ SSmicmen(year, slope, Asym))
marr_1 <- nls(S_1 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_1,  MicMen= mmic_1) #Lomolino = mlom_1, 
sapply(allmods, AIC)
pred_1 <- predict(marr_1, newdata = data.frame(year=xtmp))
marr_1; confint(marr_1)
#lines(xtmp, predict(marr_1, newdata=data.frame(year=xtmp)), lwd=2, col = 4)


#plot for Box 1 Figure 1c
par(mar = c(6, 6, 2, 1) + 0.1)
colours <- c("cadetblue4", "cadetblue3", "cadetblue2", "cadetblue1")
plot(year, S, pch = 19, ylim = c(0,max(cum.taxa.all.sites$no.taxa)+1), ylab = "Cumulative species richness", xlab = "Time (years)", bty="l",  main = "", xlim = c(0,16), col = "cadetblue4", cex=1.75, cex.axis=1.75, cex.lab=1.75)
lines(xtmp, pred_all, lwd=2, col = "cadetblue4")
points(year, S_5, col="cadetblue3", pch=19, cex=1.75)
lines(xtmp, pred_5, col="cadetblue3")
points(year, S_2, col="cadetblue2", pch=19, cex=1.75)
lines(xtmp, pred_2, col="cadetblue2")
points(year, S_1, col="cadetblue1", pch=19, cex=1.75)
lines(xtmp, pred_1, col="cadetblue1")
text(1, 70, "C", cex=2, font = 2)

par(mar = c(6, 0, 2, 1) + 0.1)
plot.new()
legend("left", fill = colours, bty = 'n', legend = c("11 sites", "5 sites","2 sites","1 site"), cex=1.75)

## accumulation over space
no.taxa.space <- cuml.taxa.space.fun(dat$comm.long)
num.tax <- no.taxa.space$no.taxa
site <- seq(1, length(unique(dat$comm.long$SITE_ID)),1)
mlom <- nls(num.tax ~ SSlomolino(site, Smax, A50, Hill)) 
mmic <- nls(num.tax ~ SSmicmen(site, slope, Asym))
marr <- nls(num.tax ~ SSarrhenius(site, k, z))

allmods <- list(Arrhenius = marr, Lomolino = mlom, MicMen= mmic)
sapply(allmods, AIC)

# Box 1 Figure 1d
par(mar = c(6, 1, 2, 1) + 0.1)
plot(rownames(no.taxa.space), no.taxa.space$no.taxa, pch = 19,  xaxt="n", bty="l", xlab = "Cumulative number of sites", ylab = "", ylim = c(0,max(no.taxa.space$no.taxa)+1), col = "cadetblue4", cex=1.75, cex.lab=1.75, cex.axis=1.75) #type = "o",
lines(site, predict(mmic, newdata=data.frame(site=site)), lwd=2, col = "cadetblue4")
axis(side=1, at = rownames(no.taxa.space), labels = seq(1,length(no.taxa.space$site),1), cex.axis=1.75)

text(1.5, 70, "D", cex=2, font = 2)

dev.off()

# FIGURE 2: Succession and species invasions ---------------------------------------------------------------------------------------------------
# PANEL A: and-plants-mtStHelens
L3dat <- read.csv("manuscripts/ms1/L3-and-plants-mtStHelens.csv", header = T, stringsAsFactors=F)

# MAKE DATA LIST
dat <- list()

# Subset out community data and add it to dat list
dat$comm.long <- subset(L3dat, OBSERVATION_TYPE=='TAXON_COUNT')

# Ensure that community data VALUE and DATE are coded as numeric
dat$comm.long <- dat$comm.long %>%   # Recode if necessary
  mutate_at(vars(c(DATE, VALUE)), as.numeric)

# Ensure that SITE_ID is a character: recode numeric as character 
dat$comm.long <- dat$comm.long %>%   # Recode if necessary
  mutate_at(vars(SITE_ID), as.character)

# Double-check that all columns are coded properly
ifelse(FALSE %in% 
         c(
           class(dat$comm.long$OBSERVATION_TYPE) == "character",
           class(dat$comm.long$SITE_ID) == "character",
           class(dat$comm.long$DATE) == "numeric",
           class(dat$comm.long$VARIABLE_NAME) == "character",
           class(dat$comm.long$VARIABLE_UNITS) == "character",
           class(dat$comm.long$VALUE) == "numeric"
         ),
       "ERROR: Community columns incorrectly coded.", 
       "OK: Community columns correctly coded.")


#make subsets of data:
sites <- unique(dat$comm.long$SITE_ID)
set.seed(12)
sites <- sample(sites,length(sites), replace=F)

one.site <- subset(dat$comm.long, SITE_ID==sites[1])
two.sites <- subset(dat$comm.long, SITE_ID %in% sites[1:2])
six.sites <- subset(dat$comm.long, SITE_ID %in% sites[1:6])

# Calculate cumulative taxa (i.e., species accumulation) across all sites pooled together
cum.taxa.all.sites <- cuml.taxa.fun(EX = dat$comm.long)
cum.taxa.one.site <- cuml.taxa.fun(EX = one.site)
cum.taxa.two.sites <- cuml.taxa.fun(EX = two.sites)
cum.taxa.six.sites <- cuml.taxa.fun(EX = six.sites)


####
##### TEST DIFFERENT SPP ACCUM MODELS ###########
#with vegan, fit species accumulation curve:
year <- cum.taxa.all.sites$year - min(cum.taxa.all.sites$year) +1
xtmp <- seq(min(year), max(year), len=51)
## all sites:
S <- cum.taxa.all.sites$no.taxa

##The Arrhenius model:(SSarrhenius) is the expression k*area^z. This is the most classical model that can be found in any textbook of ecology (and also in Dengler 2009). Parameter z is the steepness of the species-area curve, and k is the expected number of species in a unit area.
marr <- nls( S ~SSarrhenius(year, k, z))
confint(marr) #z = steepness and k = expected number of species

# Plot commented out - exploratory data analysis
#plot(S ~ year, xlab = "Time (years)", ylab = "Number of Species", ylim = c(1, max(S)))
#lines(xtmp, predict(marr, newdata=data.frame(year = xtmp)), lwd=2)

## Lomolino: using original names of the parameters (Lomolino 2000):
#The Lomolino model (SSlomolino) is Asym/(1 + slope^log(xmid/area)) (Lomolino 2000, Dengler 2009). Parameter Asym is the asymptotic maximum number of species, slope is the maximum slope of increase of richness, and xmid is the area where half of the maximum richness is achieved.
mlom <- nls(S ~ SSlomolino(year, Smax, A50, Hill))
mlom; confint(mlom)
#lines(xtmp, predict(mlom, newdata=data.frame(year=xtmp)), lwd=2, col = 4)

## Michaelis Menten:
mmic <- nls(S ~ SSmicmen(year, slope, Asym))
#lines(xtmp, predict(mmic, newdata = data.frame(year=xtmp)),lwd =2, col = 5)
mmic; confint(mmic)

## compare models (AIC)
allmods <- list(Arrhenius = marr,  Lomolino = mlom, MicMen= mmic)
sapply(allmods, AIC)
pred_all <- predict(marr, newdata = data.frame(year=xtmp))


##### FIT THE REST  OF THE  YEARS  ###########
S_1 <- cum.taxa.one.site$no.taxa
S_2 <- cum.taxa.two.sites$no.taxa
S_6 <- cum.taxa.six.sites$no.taxa

#for looking at fits
#plot(year, S, pch = 19, ylim = c(0,max(cum.taxa.all.sites$no.taxa)+1), ylab = "Cumulative species richness", xlab = "Time (years)", bty="l", xaxt="n", main = "Mt St Helens")
#lines(xtmp, pred_all, lwd=2)


#6 sites
#points(year, S_6, pch = 19, col=2)
cntrl <- nls.control(maxiter=300)
mlom_6 <- nls(S_6 ~ SSlomolino(year, Smax, A50, Hill), control = cntrl)
mmic_6 <- nls(S_6 ~ SSmicmen(year, slope, Asym))
marr_6 <- nls(S_6 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_6,  MicMen= mmic_6)
sapply(allmods, AIC)
pred_6 <- predict(marr_6, newdata = data.frame(year=xtmp))
marr_6; confint(marr_6)
#lines(xtmp, predict(marr_6, newdata=data.frame(year=xtmp)), lwd=2, col = 2)


#2 sites
#points(year, S_2, pch = 19, col=3)
mlom_2 <- nls(S_2 ~ SSlomolino(year, Smax, A50, Hill), control = cntrl)
mmic_2 <- nls(S_2 ~ SSmicmen(year, slope, Asym))
marr_2 <- nls(S_2 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_2,  Lomolino = mlom_2, MicMen= mmic_2)
sapply(allmods, AIC)
pred_2 <- predict(marr_2, newdata = data.frame(year=xtmp))
marr_2; confint(marr_2)
#lines(xtmp, predict(marr_2, newdata=data.frame(year=xtmp)), lwd=2, col = 3)


#1 site
#points(year, S_1, pch = 19, col=4)
mlom_1 <- nls(S_1 ~ SSlomolino(year, Smax, A50, Hill)) #error
mmic_1 <- nls(S_1 ~ SSmicmen(year, slope, Asym))
marr_1 <- nls(S_1 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_1,  MicMen= mmic_1, Lomolino = mlom_1) 
sapply(allmods, AIC)
pred_1 <- predict(mmic_1, newdata = data.frame(year=xtmp))
mmic_1; confint(mmic_1)
#lines(xtmp, predict(mmic_1, newdata=data.frame(year=xtmp)), lwd=2, col = 4)


#start making Figure 2a (don't plot fits, it's succession.)
pdf(file = "Box_1_Fig2-revised.pdf", height = 3.5, width = 7)
colours = c("coral4", "coral3", "coral2", "coral1")
par(mfrow = c(1,3))
par(mar = c(4, 6, 1, 1) + 0.1)
plot(year, S, pch = 19, ylim = c(0,max(cum.taxa.all.sites$no.taxa)+1), ylab = "Cumulative species richness", xlab = "Time (years)", bty="l",  main = "", type = "o", col = "coral4", cex=1.75, cex.axis=1.75, cex.lab=1.75)
#lines(xtmp, pred_all, lwd=2)
points(year, S_6, pch=19, cex=1.75, type = "o", col = "coral3")
#lines(xtmp, pred_6, col="coral3")
points(year, S_2, col="coral2", pch=19, cex=1.75, type = "o")
#lines(xtmp, pred_2, col="coral2)
points(year, S_1, col="coral1", pch=19, cex=1.75, type = "o")
#lines(xtmp, pred_1, col="coral1)
text(1.5, 70, "A", cex=2, font = 2)

par(mar = c(4, 0, 1, 1) + 0.1)
plot.new()
legend("left", fill = colours, legend = c("12 sites", "6 sites","2 sites","1 site"), cex=1.75, bty= 'n')

## accumulation over space
no.taxa.space <- cuml.taxa.space.fun(dat$comm.long)

# Box 1 Figure 2b
par(mar = c(4, 4, 1, 4) + 0.1)
plot(rownames(no.taxa.space), no.taxa.space$no.taxa, pch = 19, type = "o", xaxt="n", bty="l", xlab = "Cumulative number of sites", ylab = "", cex=1.75, ylim = c(0,max(no.taxa.space$no.taxa)+1), cex.axis=1.75, cex.lab=1.75)

axis(side=1, at = rownames(no.taxa.space), labels = seq(1,length(no.taxa.space$site),1), cex.axis=1.75)

text(1.5, 70, "B", cex=2, font = 2)

dev.off()

