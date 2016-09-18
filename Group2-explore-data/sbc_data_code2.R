# ------------------------------------------------- #
# Reformat and explor  SBC data - 18 September 2016 #
# ------------------------------------------------- #

# Set working environment
rm(list = ls())
setwd("~/Google Drive/LTER-DATA/SBC-Lamy-Castorani/")

# Check for and install required packages
library()

for (package in c('dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom', 'ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# ---------------------------------------------------------------------------------------------------
# Read in SBC data
id.coords <- "0BxUZSA1Gn1HZNUJRb0pEWkpUNFE" # Google Drive file ID
id.env <- "0BxUZSA1Gn1HZZDF5cGpnLWtnWGc" # Google Drive file ID
id.comm <- "0BxUZSA1Gn1HZUklacVFqNVhxQmM" # Google Drive file ID

# SBC site coordinates
sbc.xy <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.coords)) %>% 
  tbl_df() %>%
  gather("VARIABLE_NAME", "VALUE", LAT:LONG) %>%
  mutate(OBSERVATION_TYPE = "SPATIAL_COORDINATE",
         VARIABLE_UNITS = "dec. degrees")
sbc.xy$DATE = NA
sbc.xy$TAXON_GROUP = NA
sbc.xy <- sbc.xy[, c("OBSERVATION_TYPE", "SITE_ID", "DATE", "VARIABLE_NAME", "VARIABLE_UNITS", "VALUE", "TAXON_GROUP")] # Reorder columns

# SBC environmental data
sbc.env <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.env)) %>% 
  tbl_df() %>%
  gather("VARIABLE_NAME", "VALUE", TEMP_MEAN_C:WAVE_HT_WINTER_MEAN) %>%
  mutate(OBSERVATION_TYPE = "ENV_VAR",
         VARIABLE_UNITS = NA)
sbc.env$VARIABLE_UNITS[sbc.env$VARIABLE_NAME=="TEMP_MEAN_C"] <- "degrees C"
sbc.env$VARIABLE_UNITS[sbc.env$VARIABLE_NAME=="WAVE_HT_MEAN" | sbc.env$VARIABLE_NAME=="WAVE_HT_WINTER_MEAN"] <- "m"
sbc.env$TAXON_GROUP = NA
sbc.env <- sbc.env[, c("OBSERVATION_TYPE", "SITE_ID", "DATE", "VARIABLE_NAME", "VARIABLE_UNITS", "VALUE", "TAXON_GROUP")] # Reorder columns

# SBC community data
sbc.comm.raw <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.comm)) %>% 
  tbl_df()

sbc.comm.long <- sbc.comm.raw %>% 
  tbl_df() %>%
  mutate(VARIABLE_NAME = TAXON_CODE,
         OBSERVATION_TYPE = "TAXON_COUNT",
         VALUE = BIOMASS_DENS,
         VARIABLE_UNITS = "g dry per m2") %>%
  select(-TAXON_GENUS, -TAXON_SPECIES, -BIOMASS_DENS, -TAXON_CODE) 
sbc.comm.long <- sbc.comm.long[, c("OBSERVATION_TYPE", "SITE_ID", "DATE", "VARIABLE_NAME", "VARIABLE_UNITS", "VALUE", "TAXON_GROUP")] # Reorder columns

# Bind data together in a single long data frame
sbc.long <- rbind(sbc.xy, sbc.env, sbc.comm.long)
dim(sbc.long)

# Save long-form combined data
#write.csv(sbc.long, file="sbc_long_dat.csv")

# ---------------------------------------------------------------------------------------------------
# EXAMINE SPATIOTEMPORAL NATURE OF COMMUNITY DATA

# Rename data
dat.long <- sbc.long

# Check types of taxa
unique(dat.long$TAXON_GROUP)

# Subset data
dat.long.2 <- subset(dat.long, dat.long$TAXON_GROUP != "")
#dat.long.2 <- subset(dat.long, dat.long$TAXON_GROUP != "FISH" & dat.long$TAXON_GROUP != "MOBILE INVERT" & dat.long$TAXON_GROUP != "") # Remove mobile groups
dat.long.2 <- droplevels(dat.long.2)

# Check the structure of the data
str(dat.long.2)

# Check sites
unique(dat.long.2$SITE_ID)

# Check dates
unique(dat.long.2$DATE)

# Check for numbers of observations at each site and year
tapply(dat.long.2$VALUE, list(dat.long.2$SITE_ID,dat.long.2$DATE), length)
# Are all taxa propagated throughout the data? In other words, when there were no individuals found, was a zero recorded in the data frame?

# Check temporal nature of the data
# Add information here if needed for non-annual data

# Subset long data to community data only
comm.dat <- dat.long.2[dat.long.2$OBSERVATION_TYPE == "TAXON_COUNT", ]

# ---------------------------------------------------------------------------------------------------
# NUMBER OF OBSERVED TAXA THROUGH TIME

# Examine temporal patterns in observations of the number of species
no.taxa.fun <- function(community.data) {
  # Number of taxa at each site through time
   no.taxa <- community.data %>%
    filter(VALUE > 0) %>%
    select(DATE, VARIABLE_NAME, SITE_ID) %>%
    unique() %>%
    mutate(no.taxa = 1) %>%
    group_by(SITE_ID, DATE) %>%
    summarize(no.taxa = sum(no.taxa))
  
   # Combined number of taxa among all sites through time
  total.no.taxa <- community.data %>%
    filter(VALUE > 0) %>%
    select(DATE, VARIABLE_NAME) %>%
    unique() %>%
    mutate(no.taxa = 1) %>%
    group_by(DATE) %>%
    summarize(no.taxa = sum(no.taxa))
  
  return(list("no.taxa" = no.taxa, "total.no.taxa" = total.no.taxa))
}

no.taxa <- no.taxa.fun(comm.dat)

# Check structure of no.taxa
str(no.taxa)

# Plot number of taxa through time
ggplot(data=no.taxa$no.taxa, aes(x=DATE, y=no.taxa)) +
  geom_line(aes(color=SITE_ID)) +
  geom_line(data=no.taxa$total.no.taxa, aes(x=DATE, y=no.taxa), color="black", size=1) +
  xlab("Year") +
  ylab("Number of taxa observed") +
  ylim(c(0, max(no.taxa$total.no.taxa$no.taxa))) +
  theme_bw()

# ---------------------------------------------------------------------------------------------------
# CUMULATIVE NUMBER OF SPECIES

# Make a function that returns the cumulative number of taxa observed for a given set of community data
cuml.taxa.fun <- function(community.data){
  taxa.t.list <- list() # Make empty list
  
  # Loop over each year, creating a list that contains the unique taxa found in each year
  for(t in 1:length(unique(community.data$DATE))){
    tmp.dat <- subset(community.data, community.data$DATE == t + (min(community.data$DATE) - 1))
    tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0) 
    taxa.t.list[[t]] <- unique(tmp.dat.pres$VARIABLE_NAME)
  }
  
  # Make cumulative list of taxa through time
  cuml.taxa <- list() # Empty list
  cuml.taxa[[1]] <- taxa.t.list[[1]] # Add the taxa from the first time step 
  
  # Run for-loop to create list of the cumulative taxa, with duplicates
  for(t in 2:length(unique(community.data$DATE))){ 
    cuml.taxa[[t]] <- c(cuml.taxa[[t - 1]], taxa.t.list[[t]])
  }
  
  # Remove duplicates
  cuml.taxa <- lapply(cuml.taxa, function(x){unique(x)})
  
  # Return the number of total unique taxa through time
  cuml.no.taxa <- data.frame("year" = unique(community.data$DATE))
  cuml.no.taxa$no.taxa <- unlist(lapply(cuml.taxa, function(x){length(x)}))
  
  return(cuml.no.taxa)
  }

# Run for all sites
cuml.taxa.all.sites <- cuml.taxa.fun(community.data = comm.dat)

# Plot the cumulative number of species observed across all sites
ggplot(data=cuml.taxa.all.sites, aes(x=year, y=no.taxa)) +
  geom_line() +
  xlab("Year") +
  ylab("Cumulative number of taxa") +
  ylim(c(0, max(cuml.taxa.all.sites$no.taxa))) +
  theme_bw()

# Examine site-level patterns
## sort the comm.dat dataframe to make sure its ordered by site
comm.dat <- comm.dat %>%
  arrange(SITE_ID)

## split the data frame, and apply the cuml.taxa.fun() for each site
X <- split(comm.dat, comm.dat$SITE_ID)
out <- lapply(X, cuml.taxa.fun)

# make the lists a dataframe
output <- do.call("rbind", out)

# extract rownames to create a SITE_ID column
output$rnames <- row.names(output)

# clean up the SITE_ID column
cuml.taxa.by.site <- output %>%
  tbl_df() %>%
  separate(rnames, c("SITE_ID", "todrop")) %>%
  select(-todrop)

# Plot the cumulative number of species observed at each site
ggplot(data=cuml.taxa.by.site, aes(x = year, y = no.taxa)) +
  geom_line(aes(color = SITE_ID)) +
  geom_line(data = cuml.taxa.all.sites, aes(x=year, y=no.taxa), size = 1.5) +
  xlab("Year") +
  ylab("Cumulative number of taxa") +
  ylim(c(0, max(cuml.taxa.all.sites$no.taxa))) +
  theme_bw()

# ---------------------------------------------------------------------------------------------------
# RANK ABUNDANCE CURVES