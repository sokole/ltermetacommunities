# ---------------------------------------------- #
# Explore environmental data - 18 September 2016 #
# ---------------------------------------------- #

# Clear environment
rm(list = ls())

# Assign data set of interest
data.set <- "SBC-Lamy-Castorani"
data.key <- "0BxUZSA1Gn1HZYTVfd2FZTWhWbm8" # Google Drive file ID (different for each dataset)

# Set working environment
setwd(paste("~/Google Drive/LTER-DATA/", data.set, sep=""))

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

# Create data frame for community data only (drop spatial and environmental data)
comm.dat <- dat.long[dat.long$OBSERVATION_TYPE == "TAXON_COUNT", ]

# Subset data if necessary
#comm.dat <- subset(comm.dat, comm.dat$TAXON_GROUP != "INSERT NAME OF REMOVAL GROUP HERE")
#comm.dat <- droplevels(comm.dat)

# ---------------------------------------------------------------------------------------------------
# CHECK DATA STRUCTURE AND SPATIOTEMPORAL SAMPLING EFFORT

# Data structure
str(comm.dat)

# How are sites sampled across time?
ggplot(data = comm.dat, aes(x = DATE, y = SITE_ID)) +
  geom_raster() +
  theme_bw() +
  xlab("Year") +
  ylab("Site") 

# Propagation of species across space and time
tapply(comm.dat$VALUE, list(comm.dat$SITE_ID,comm.dat$DATE), length)

# ---------------------------------------------------------------------------------------------------
# ALPHA DIVERSITY (SPECIES RICHNESS) OVER TIME AND SPACE

# Examine temporal patterns in observations of the number of species
no.taxa.fun <- function(dat) {
  
  # Number of unique taxa at each site through time
   no.taxa <- dat %>%
    filter(VALUE > 0) %>%
    select(DATE, VARIABLE_NAME, SITE_ID) %>%
    unique() %>%
    mutate(no.taxa = 1) %>%
    group_by(SITE_ID, DATE) %>%
    summarize(no.taxa = sum(no.taxa))
  
   # Summed number of unique taxa among all sites through time
  total.no.taxa <- dat %>%
    filter(VALUE > 0) %>%
    select(DATE, VARIABLE_NAME) %>%
    unique() %>%
    mutate(no.taxa = 1) %>%
    group_by(DATE) %>%
    summarize(no.taxa = sum(no.taxa))
  
  return(list("no.taxa" = no.taxa, "total.no.taxa" = total.no.taxa))
}

# Apply function across community data
no.taxa <- no.taxa.fun(comm.dat) # Result is a list of: (1) no. of taxa at each site; (2) no. of taxa at all sites

# Plot a heatmap of the number of species observed over space and time
ggplot(data = no.taxa$no.taxa, aes(x = DATE, y = SITE_ID, fill = no.taxa)) +
  geom_raster() +
  theme_bw() +
  guides(fill = guide_legend(title = "Number of taxa")) +
  xlab("Year") +
  ylab("Site") +
  theme(aspect.ratio = 1)

# Plot number of taxa through time
ggplot(data=no.taxa$no.taxa, aes(x=DATE, y=no.taxa)) +
  geom_line(aes(color=SITE_ID)) +
  geom_line(data=no.taxa$total.no.taxa, aes(x=DATE, y=no.taxa), color="black", size=1) +
  xlab("Year") +
  ylab("Number of taxa observed") +
  guides(color = guide_legend(title = "Site")) +
  ylim(c(0, max(no.taxa$total.no.taxa$no.taxa))) +
  theme_bw() 
# Note that the thick line indicates the total number of taxa among all sites

# ---------------------------------------------------------------------------------------------------
# SITE-SPECIFIC AND TOTAL SPECIES ACCUMULATION CURVES

# Make a function that returns the cumulative number of taxa observed for a given set of community data
cuml.taxa.fun <- function(dat){
  taxa.t.list <- list() # Make empty list
  
  # Loop over each year, creating a list that contains the unique taxa found in each year
  for(t in 1:length(unique(dat$DATE))){
    tmp.dat <- subset(dat, dat$DATE == t + (min(dat$DATE) - 1))
    tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0) 
    taxa.t.list[[t]] <- unique(tmp.dat.pres$VARIABLE_NAME)
  }
  
  # Make cumulative list of taxa through time
  cuml.taxa <- list() # Empty list
  cuml.taxa[[1]] <- taxa.t.list[[1]] # Add the taxa from the first time step 
  
  # Run for-loop to create list of the cumulative taxa, with duplicates
  for(t in 2:length(unique(dat$DATE))){ 
    cuml.taxa[[t]] <- c(cuml.taxa[[t - 1]], taxa.t.list[[t]])
  }
  
  # Remove duplicates
  cuml.taxa <- lapply(cuml.taxa, function(x){unique(x)})
  
  # Return the number of total unique taxa through time
  cuml.no.taxa <- data.frame("year" = unique(dat$DATE))
  cuml.no.taxa$no.taxa <- unlist(lapply(cuml.taxa, function(x){length(x)}))
  
  return(cuml.no.taxa)
  }

# Calculate cumulative taxa (i.e., species accumulation) across all sites pooled together
cuml.taxa.all.sites <- cuml.taxa.fun(dat = comm.dat)

# Examine site-level patterns of species accumulation
# First, sort the comm.dat dataframe to make sure its ordered by site
comm.dat <- comm.dat %>% 
  arrange(SITE_ID)

# Then, split the data frame, and apply the cuml.taxa.fun() for each site
X <- split(comm.dat, comm.dat$SITE_ID)
out <- lapply(X, cuml.taxa.fun)

# Make the lists a dataframe
output <- do.call("rbind", out)

# Extract rownames to create a SITE_ID column
output$rnames <- row.names(output)

# Clean up the SITE_ID column
cuml.taxa.by.site <- output %>%
  tbl_df() %>%
  separate(rnames, c("SITE_ID", "todrop")) %>%
  select(-todrop)

# Plot the cumulative number of taxa observed at each site, as well as across all sites together
ggplot(data=cuml.taxa.by.site, aes(x = year, y = no.taxa)) +
  geom_line(aes(color = SITE_ID)) +
  geom_line(data = cuml.taxa.all.sites, aes(x=year, y=no.taxa), size = 1.5) +
  xlab("Year") +
  ylab("Cumulative number of taxa") +
  guides(color = guide_legend(title = "Site")) +
  ylim(c(0, max(cuml.taxa.all.sites$no.taxa))) +
  theme_bw()
# Note that the thick line indicates the total number of taxa among all sites

# ---------------------------------------------------------------------------------------------------
# RANK ABUNDANCE CURVES

# More to do here! :)
