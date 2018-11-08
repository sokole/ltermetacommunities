# --------------------------------------------------------- #
# Format raw data                       #
# FCE diatoms                                                       #
# Revised 01 Jun 2017                                       #
# --------------------------------------------------------- #

# Contributors: Aldo Compagnoni, Chris Catano, Riley Andrade, Max Castorani, Nina Lany, Sydne Record, Nicole Voelker

# Clear environment
rm(list = ls())

#Check to make sure working directory is correct
if(basename(getwd())!="ltermetacommunities"){cat("Plz change your working directory. It should be 'ltermetacommunities'")}

# Check for and install required packages
for (package in c('dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom', 'ggplot2', 'testthat')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


# Aldo's March 2017 formating code FCE env data ----------------------------------
options( stringsAsFactors = F )

# get data keys
key_env      <- '1W4jX2Nna1sYJF7L4LaRX8hOK0AcP9Zlc'
key_diat     <- '1AtXkM-fBhoHXj-d_sTwIFmi8uZd8umrg'
key_env_nam  <- '1Un2DA-CCraid6tymLIOZRYvPhMPdTHB0'

# download data frames
fec_env      <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", key_env))
fec_diat     <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", key_diat))
env_names    <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", key_env_nam))

# format diatom data
vars <- c("OBSERVATION_TYPE","SITE_ID","DATE",
          "VARIABLE_NAME","VARIABLE_UNITS","VALUE")

# coordinates ----------------------------------------------------------------

# find a centroid for each PSU
centr   <- fec_env %>%
            mutate(PSU = as.factor(PSU) ) %>%
            select(PSU, EASTING, NORTHING) %>%
            group_by(PSU) %>%
            summarise( east_centroid = mean(EASTING),
                       north_centroid = mean(NORTHING) )

# check we didn't loose the PSU values (through "as.factor")
expect_equal(length(intersect(unique(as.character(fec_env$PSU)),
                       as.character(centr$PSU))),
             length(unique(fec_env$PSU)))


# coordinates data frame
coord <- data.frame( OBSERVATION_TYPE = "SPATIAL_COORDINATE",
                      SITE_ID = as.character(centr$PSU),
                      DATE = NA,
                      VARIABLE_NAME = "EASTING",
                      VARIABLE_UNITS = "METERS",
                      VALUE = centr$east_centroid) %>%
            rbind( data.frame( OBSERVATION_TYPE = "SPATIAL_COORDINATE",
                               SITE_ID = as.character(centr$PSU),
                               DATE = NA,
                               VARIABLE_NAME = "NORTHING",
                               VARIABLE_UNITS = "METERS",
                               VALUE = centr$north_centroid)
            )



# taxon count 

# calculate mean of diatom species across samples
# (NOT NEEDED, number of rows does not change)
mean_diat <- fec_diat %>% 
                # # modify the one instance of sampling in january
                # mutate( Date = replace(Date, 
                #          grepl('^1/[0-9]{1,2}/2011', Date),
                #          '12/31/2010') ) %>% 
                select(Year, PSU, ACBREBRE:UNKNVALV) %>%
                group_by(Year, PSU) %>%
                summarise_all( funs(mean) )

# test equality of two data sets
expect_equal(nrow(mean_diat), nrow(fec_diat) )

# long-form this data set
diat_long <- fec_diat %>%
              select(Year, PSU, ACBREBRE:UNKNVALV) %>%
              gather(VARIABLE_NAME, VALUE,-Year,-PSU) %>%
              mutate(Year = Year + 2000,
                     VARIABLE_UNITS = "PERCENT",
                     OBSERVATION_TYPE = "TAXON_RELATIVE_ABUNDANCE") %>%
              rename(DATE = Year,
                     SITE_ID = PSU) %>%
              select_(.dots = vars)
  


# environmental variables 

# convert from character to numeric
env_mat   <- fec_env %>%
              select(Depth_cm:Chl.a..µg.m2.) %>%
              cbind( select(fec_env, PLT_COVER:HYDROPERIOD_MEAN) )

# identify character columns
classes   <- env_mat %>%
              lapply(class) %>%
              unlist() %>%
              as.character()

# grep variables which are columns
char_id   <- which(classes == "character")

# manually inspect each of 22 vars
unique(env_mat[,char_id[22]])

# introduce NAs in character columsn
# PLUS, remove ","
for(i in seq_along(char_id)){
  
  # introduce NAs
  env_mat[,char_id[i]] <- gsub("NS", "NA", env_mat[,char_id[i]])
  env_mat[,char_id[i]] <- gsub("ns", "NA", env_mat[,char_id[i]])
  env_mat[,char_id[i]] <- gsub("9999", "NA", env_mat[,char_id[i]])
  env_mat[,char_id[i]] <- gsub("8888", "NA", env_mat[,char_id[i]])
  env_mat[,char_id[i]] <- gsub("7777", "NA", env_mat[,char_id[i]])
  env_mat[,char_id[i]] <- gsub("6666", "NA", env_mat[,char_id[i]])
  env_mat[,char_id[i]] <- gsub("Z", "NA", env_mat[,char_id[i]])
  env_mat[,char_id[i]] <- gsub("UP", "NA", env_mat[,char_id[i]])
  
  # remove commas
  env_mat[,char_id[i]] <-gsub(",", "", env_mat[,char_id[i]])
  
  # convert to numeric
  env_mat[,char_id[i]] <- as.numeric( env_mat[,char_id[i]] )
  
}

# Remove non-numeric variables
non_num_vars <- c("Sample.Quantitative...Y.or.N.",
                  "Plant.Sp1","Plant.Sp2","Plant.Sp3",
                  "Floating.Sp1","Floating.Sp2","Green.Sp1")
env_n_mat    <- env_mat[,-which(names(env_mat) %in% non_num_vars)]

# introduce NAs in numeric fields
for(i in 1:ncol(env_n_mat)){

  env_n_mat[env_n_mat[,i]==9999 & 
            !is.na(env_n_mat[,i]), i] <- NA
  
}
 
# means across replicates
mean_env <- fec_env %>%
              select(Year, PSU) %>%
              cbind(env_n_mat) %>%
              group_by(Year, PSU) %>%
              summarise_all(funs(mean))

# Environment in long form
env_pre  <- mean_env %>%
              as.data.frame() %>%
              gather(VARIABLE_NAME, VALUE,-Year,-PSU) %>%
              mutate(Year = Year + 2000,
                     OBSERVATION_TYPE = "ENV_VAR") %>%
              rename(DATE = Year,
                     SITE_ID = PSU)
              

# change env_pre variable names to match with file with var. units
# Change env_names column names
env_names    <- setNames(env_names, c("VARIABLE_NAME", "VARIABLE_UNITS"))
# function to update 
sub_var_name <- function(x, string){
  x   <- gsub(string,"",x)
  return(x)
}
env_pre$VARIABLE_NAME <- sub_var_name(env_pre$VARIABLE_NAME,"_µs.cm")
env_pre$VARIABLE_NAME <- sub_var_name(env_pre$VARIABLE_NAME,"_cm")
#note regular expression trick in following line
env_pre$VARIABLE_NAME <- sub_var_name(env_pre$VARIABLE_NAME,"_C$") 
env_pre$VARIABLE_NAME <- sub_var_name(env_pre$VARIABLE_NAME,"_mL")
env_pre$VARIABLE_NAME <- sub_var_name(env_pre$VARIABLE_NAME,"µg.m2.")

# Test there is not differences
expect_equal(length(setdiff(unique(env_pre$VARIABLE_NAME), unique(env_names$VARIABLE_NAME))),
             0)

# finally, env. data in long form 
env_long <- env_names %>% 
              merge(env_pre) %>%
              select_(.dots = vars)


# put it all together
dat.long <- Reduce(function(...) rbind(...),
                   list(coord, diat_long, env_long)) %>%
                # convert SITE_ID to numeric
                mutate( SITE_ID = as.numeric(SITE_ID) )


# PART coded by Chris  ---------------------------------------------------------------------------------------------------

# Test that file above correspond to that stored on google drive
data.set <- "FCE-diatoms"
data.key <- "0B-HySt4HfBxBM0FxbVRERGtBUlk" # Google Drive file ID 

#Google Drive File Stream read in data:
#dat.long <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/FCE-diatoms-Gaiser-Marazzi/FCE_diatoms_environment_long.csv")

# Importa data to test
dat.test <-  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", data.key),
                      stringsAsFactors = F)

# test passed, we can go ahead!
expect_true( all.equal(dat.long, dat.test) )


str(dat.long)
levels(dat.long$OBSERVATION_TYPE)

# Change 'TAXON_RELATIVE_ABUNDANCE' to 'TAXON_COUNT'
dat.long$OBSERVATION_TYPE <- gsub("TAXON_RELATIVE_ABUNDANCE", "TAXON_COUNT", dat.long$OBSERVATION_TYPE)

# Write this to the L3 folder in Google Drive 
#write.csv(dat.long, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-fce-diatoms-marazzi.csv")


# MAKE DATA LIST
dat <- list()

# COMMUNITY DATA 
comm.long <- dat.long[dat.long$OBSERVATION_TYPE == "TAXON_COUNT", ] 
comm.long <- comm.long %>%
  droplevels()

str(comm.long)  # Inspect the structure of the community data

# Add number of unique taxa and number of years to data list:
dat$n.spp <- length(levels(comm.long$VARIABLE_NAME))
dat$n.years <- length(unique(comm.long$DATE))

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

#-------------------------------------------------------------------------------
# IMPORT DIATOM DATASET THAT INCLUDES REGION AND PSU CODES. WILL SUBSET LONG
# DATA TO ONLY INCLUDE SAMPLES FOR PSUs IN SRS AND TSL

data.key2 <-  "1AtXkM-fBhoHXj-d_sTwIFmi8uZd8umrg"

diatom_sites <-  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", data.key2))
unique(diatom_sites$Region)

# get vector of PSUs that are in Regions SRS and TSL
ENP_sites <- diatom_sites[diatom_sites$Region == "SRS" | diatom_sites$Region == "TSL",
                          "PSU"]

comm.long.ENP <- comm.long[comm.long$SITE_ID %in% ENP_sites, ] # records dropped


# checking for weird taxa names and removing suspicious ones that are rare
comm.long_gamma_summary <- comm.long.ENP %>% group_by(VARIABLE_NAME) %>% 
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
keep <- comm.long_gamma_summary_remove_unk$VARIABLE_NAME
comm.long.ENP2 <- comm.long.ENP[comm.long.ENP$VARIABLE_NAME %in% keep, ] # records dropped
length(unique(comm.long.ENP2$VARIABLE_NAME)) # removed 2 species (UNKNGIRD, UNKNVALV)


# ---------------------------------------------------------------------------------------------------
# Check balanced sampling of species across space and time by inspecting table, and add to data list

if(length(unique(xtabs(~ SITE_ID + DATE, data = comm.long.ENP2))) > 1){
  comm.long2 <- comm.long.ENP2 %>%
    spread(VARIABLE_NAME, VALUE, fill = 0) %>%
    gather('VARIABLE_NAME','VALUE', -c(SITE_ID, DATE, VARIABLE_UNITS))
}

(cont.table <- xtabs(~ SITE_ID + DATE, data = comm.long2)) #number of taxa should be same in all
cont.table <- as.data.frame(cont.table)
hist(na.omit(comm.long2$DATE)) #frequency should be same (even distribution)


ifelse(length(unique(xtabs(~ SITE_ID + DATE, data = comm.long.ENP2))) == 1,
       "OK: Equal number of taxa recorded across space and time.", 
       "ERROR: Unequal numbers of observations across space and time, or taxa list not fully propagated across space and time. Inspect contingency table.")

# keep sites that have same sample coverage/spacing (remove those with gaps)
a <- cont.table[cont.table$Freq == 0, ]
irregular <- unique(a$SITE_ID)
comm.long.ENP3 <- comm.long.ENP2[!(comm.long.ENP2$SITE_ID %in% irregular), ]

xtabs(~ SITE_ID + DATE, data = comm.long.ENP3) #number of taxa should be same in all
hist(na.omit(comm.long.ENP3$DATE)) #frequency should be same (even distribution)



# ---------------------------------------------------------------------------------------------------
# Convert community data to wide form
comm.wide <- comm.long.ENP3 %>%
  select(-VARIABLE_UNITS) %>%
  spread(VARIABLE_NAME,  VALUE)

# check to make sure each species is sampled at least once
zero <- colSums(comm.wide[, c(4:268)]) == 0
zero <- as.data.frame(zero)
zero$species <- rownames(zero)

# Remove species with zero occurence (zero = True)
zero2 <- zero[!(zero$zero == TRUE), ]
species <- zero2[, -1]

# long form data
dat.long.ENP <- comm.long.ENP3[comm.long.ENP3$VARIABLE_NAME %in% species, ] # records dropped

# Convert community data to wide form
comm.wide.ENP <- dat.long.ENP %>%
  select(-VARIABLE_UNITS) %>%
  spread(VARIABLE_NAME,  VALUE)

length(unique(dat.long.ENP$SITE_ID)) # 30 sites 
unique(dat.long.ENP$DATE) # 7 years (2005-2011)
length(unique(dat.long.ENP$VARIABLE_NAME)) # 192 species


write.csv(dat.long.ENP, 
          file = '~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-fce-diatoms-catano.csv', 
          row.names = F)








#--------------------------------------------------------

# no spatial or environmental data; did not run code below
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
cord <- filter(dat.long, OBSERVATION_TYPE=="SPATIAL_COORDINATE");head(cord)
cord$SITE_ID <- toupper(cord$SITE_ID)  # Ensure sites are in all caps
cord <- droplevels(cord)
str(cord)

cord.wide <- cord %>%
  select(-VARIABLE_UNITS) %>%
  spread(VARIABLE_NAME,  VALUE)

head(cord.wide)

sites <- c(unique(cord.wide$SITE_ID));sites

cord.wide$latitude <- as.numeric(as.character(cord.wide$NORTHING)) 
cord.wide$longitude <- as.numeric(as.character(cord.wide$EASTING))
cord.wide <- cord.wide[c("longitude", "latitude")] #pull last two columns and reorder

#add number of sites and long/lat coords to data list:
dat$n.sites <- length(sites)


#make data spatially explicit
coordinates(cord.wide) = c("longitude", "latitude") 
crs.geo <- CRS("+proj=utm +zone=17 +datum=WGS84") #this zone is a guess
proj4string(cord.wide) <- crs.geo  # define projection system of our data to WGS84 
summary(cord.wide) 


#if DATA IS IN UTM OR OTHER KNOWN COORDINATE SYSTEM YOU CAN TRANSFORM IT, EG... UTM data for PHX and NWT 
cord.wide <- spTransform(cord.wide, CRS("+proj=longlat")) 
summary(cord.wide) #check transformation

#for plotting in ggplot later
dat$longlat <- as.data.frame(cord.wide)

#create a distance matrix between sites, best fit distance function TBD
distance.mat <- (distm(cord.wide, fun = distVincentyEllipsoid)/1000) #km distance
rownames(distance.mat) <- sites
colnames(distance.mat) <- sites

#add distance matrix to data list and maximum distance between sites (km) to data list
dat$distance.mat <- distance.mat
dat$max.distance <- max(distance.mat)
summary(dat)

# ---------------------------------------------------------------------------------------------------
# ENVIRONMENTAL COVARIATES
env.long <- subset(dat.long, OBSERVATION_TYPE == "ENV_VAR")
env.long <- droplevels(env.long)
str(env.long)
levels(env.long$VARIABLE_NAME)
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

# Are community data balanced over space and time?
ifelse(nrow(dat$comm.wide) == dat$n.years * dat$n.sites, "Yes", "No")

# Inspect summary of 'dat' list 
summary(dat)

#clean up the workspace
rm("comm.long","comm.wide","cord","cord.wide","crs.geo","dat.long", "data.key", "data.set","distance.mat","env.long", "env.wide","package","sites")
ls()




