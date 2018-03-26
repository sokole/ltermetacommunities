# ---------------------------------------- #
# Explore community data - 30 Mar 2017     #
# ---------------------------------------- #

# Clear environment
rm(list = ls())

# Make sure your working environment is set to the GitHub repository ltermetacommunities. 
#Check to make sure working directory is correct
if(basename(getwd())!="ltermetacommunities"){cat("Plz change your working directory. It should be 'ltermetacommunities'")}


# Check for and install required packages
#library()

for (package in c('dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom', 'ggplot2', 'iNEXT', 'grDevices', 'RColorBrewer','BiodiversityR')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# ---------------------------------------------------------------------------------------------------
# Assign L3 data set of interest
# NOTE: Google Drive file ID is different for each dataset

#datasets mostly ready (may need further subsetting prior to analysis)
# CSUN-USVI-coral
data.set <- "usvi-coral-castorani"
data.key <- "0BxUZSA1Gn1HZZGowdUVCTTdtXzg" # Google Drive file ID

# mcr-algae-castorani
data.set <- "mcr-algae-castorani"
data.key <- "0BxUZSA1Gn1HZenhxaVJ6bWtVdDg" # Google Drive file ID

# mcr-coral-castorani
data.set <- "mcr-coral-castorani"
data.key <- "" # Google Drive file ID

# mcr-inverts-castorani
data.set <- "mcr-inverts-castorani"
data.key <- "" # Google Drive file ID

# mcr-fish-castorani
data.set <- "mcr-fish-castorani"
data.key <- "" # Google Drive file ID

# sbc-algae-castorani (survey)
data.set <- "sbc-algae-castorani"
data.key <- "0BxUZSA1Gn1HZZWl6d3BMeVNlT0U" # Google Drive file ID

# sbc-fish-castorani (survey)
data.set <- "sbc-fish-castorani"
data.key <- "" # Google Drive file ID

# sbc-mobile_inverts-castorani (survey)
data.set <- "sbc-mobileInverts-castorani"
data.key <- "" # Google Drive file ID

# sbc-sessile_inverts-castorani (survey)
data.set <- "sbc-sessileInverts-castorani"
data.key <- "" # Google Drive file ID

# jrn-lizard-hope 
data.set <- "jrn-lizards-hope"
data.key <- "0B7o8j0RLpcxiYW10X1djMTBGM0U" # Google Drive file ID 

# nwt-plants-hallett 
data.set <- "nwt-plants-hallett"
data.key <- "0B2P104M94skvQzE2QUMtNHpCcXc" # Google Drive file ID 

# mcm-diatoms-schulteSokol 
data.set <- "mcm-diatoms-schulteSokol"
data.key <- "" # Google Drive file ID 

#datasets that need significant work still:

# ntl-fish-stanleyLottig (not fully propogated... need to add in zeroes when not observed! Figs are in the tex file)
data.set <- "ntl-fish-stanleyLottig"
data.key <- "" # Google Drive file ID

# ntl-macroinvertebrates-stanleyLottig (keys for taxa need to be edited - Chaoborus larvae and Chaobotus pupae are given different codes. Also remove 1998 due to incomplete sampling. Also, there are only 4 taxa... too few for analysis?)
data.set <- "ntl-macroinvertebrate-stanleyLottig"
data.key <- "" # Google Drive file ID

# ntl-zooplankton-stanleyLottig (Tr and TR may be the same site?)
data.set <- "ntl-zooplankton-stanleyLottig"
data.key <- "" # Google Drive file ID

# fce-algae-marazzi (contains duplicated records... double check) Some figs not made yet. (remove a year; very unbalanced)
data.set <- "fce-algae-marazzi"
data.key <- "0B2P104M94skvbVdsYUc4amdSLWc" # Google Drive file ID

# fce-diatoms-marazzi (very unbalanced)
data.set <- "fce-diatoms-marazzi"
data.key <- "" # Google Drive file ID
 

# sbc-fish-castorani (survey) Six extra sites sampled in the last year. Need to learn whether to remove them or aggregate them in with the appropriate transect.
data.set <- "sbc-fish-castorani"
data.key <- "" # Google Drive file ID

#----------------------------------------------------------------------------------------------------
# MAKE DATA LIST
dat <- list()

# IMPORT DATA
L3dat <-  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", data.key), stringsAsFactors=F) 

#Work around if no internet access, here's a workaround assuming the Google Drive directory is stored locally:
L3dat <- read.csv(paste("~/Google Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-", data.set, ".csv", sep=""), stringsAsFactors=F)


# Subset out community data  and add it to dat list
dat$comm.long <- subset(L3dat, OBSERVATION_TYPE=='TAXON_COUNT')

# Convert community data to wide form
comm.wide <- dat$comm.long %>%
  select(-VARIABLE_UNITS) %>%
  spread(VARIABLE_NAME,  VALUE)

dat$comm.wide <- comm.wide
summary(dat)

#---------------------------------------------------------------------------------------------------
# Create color palette for heatmaps
heat.pal.spectral <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

#---------------------------------------------------------------------------------------------------
# CHECK DATA STRUCTURE AND SPATIOTEMPORAL SAMPLING EFFORT

# How are communities sampled across space and time?
# Write spatiotemporal sampling effort plot to pdf 
pdf(file=paste('~/Google Drive/LTER Metacommunities/Manuscripts/MS3_metacom-stability-analysis/supplemental_methods_figs/', data.set, '_spatiotemporal_sampling_effort.pdf',sep=''))
  ggplot(data = dat$comm.long, aes(x = DATE, y = SITE_ID)) +
  geom_point(size = 5) +
  theme_bw() +
  xlab("Year") +
  ylab("Site") +
  theme_bw() +
  theme(axis.title = element_text(size=20), axis.text = element_text(size=20))
dev.off()

# Check the propagation of species across space and time
tapply(dat$comm.long$VALUE, list(dat$comm.long$SITE_ID,dat$comm.long$DATE), length)

#---------------------------------------------------------------------------------------------------
# ALPHA DIVERSITY (SPECIES RICHNESS) OVER TIME AND SPACE

# Examine temporal patterns in observations of the number of species
no.taxa.fun <- function(ex) {
  
  # Number of unique taxa at each site through time
   no.taxa <- ex %>%
    filter(VALUE > 0) %>%
    select(DATE, VARIABLE_NAME, SITE_ID) %>%
    unique() %>%
    mutate(no.taxa = 1) %>%
    group_by(SITE_ID, DATE) %>%
    summarize(no.taxa = sum(no.taxa))
  
   # Summed number of unique taxa among all sites through time
  total.no.taxa <- ex %>%
    filter(VALUE > 0) %>%
    select(DATE, VARIABLE_NAME) %>%
    unique() %>%
    mutate(no.taxa = 1) %>%
    group_by(DATE) %>%
    summarize(no.taxa = sum(no.taxa))
  
  return(list("no.taxa" = no.taxa, "total.no.taxa" = total.no.taxa))
}

# Apply function across community data
no.taxa <- no.taxa.fun(dat$comm.long) # Result is a list of: (1) no. of taxa at each site; (2) no. of taxa at all sites

# Plot a heatmap of the number of species observed over space and time
ggplot(data = no.taxa$no.taxa, aes(x = DATE, y = SITE_ID, fill = no.taxa)) +
  geom_raster() +
  scale_fill_gradientn(colours = heat.pal.spectral(100), name = "No. of taxa") +
  theme_bw() +
  #guides(fill = guide_legend(title = "Number of taxa")) +
  xlab("Year") +
  ylab("Site") +
  theme(aspect.ratio = 1)


# Plot number of taxa through time
# Note that the thick line indicates the total number of taxa among all sites
#write to file:
pdf(file=paste('~/Google Drive/LTER Metacommunities/Manuscripts/MS3_metacom-stability-analysis/supplemental_methods_figs/', data.set, '_num_taxa_over_time.pdf',sep=''))
ggplot(data=no.taxa$no.taxa, aes(x=DATE, y=no.taxa)) +
  geom_point(aes(color = SITE_ID)) +
  geom_line(aes(color=SITE_ID)) +
  geom_point(data=no.taxa$total.no.taxa, aes(x=DATE, y=no.taxa), color="black", size=3) +
  geom_line(data=no.taxa$total.no.taxa, aes(x=DATE, y=no.taxa), color="black", size=1) +
  xlab("Year") +
  ylab("Number of taxa observed") +
  guides(color = guide_legend(title = "Site")) +
  ylim(c(0, max(no.taxa$total.no.taxa$no.taxa))) +
  theme_bw() +
  theme(axis.title = element_text(size=20), axis.text = element_text(size=20))
  dev.off()
# ---------------------------------------------------------------------------------------------------
# SITE-SPECIFIC AND TOTAL SPECIES ACCUMULATION CURVES

# Make a function that returns the cumulative number of taxa observed for a given set of community data
cuml.taxa.fun <- function(EX){
  taxa.t.list <- list() # Make empty list
  
  # Loop over each year, creating a list that contains the unique taxa found in each year
  for(t in 1:length(unique(EX$DATE))){
    tmp.dat <- subset(EX, EX$DATE == t + (min(EX$DATE) - 1))
    tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0) 
    taxa.t.list[[t]] <- unique(tmp.dat.pres$VARIABLE_NAME)
  }
  
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

# Calculate cumulative taxa (i.e., species accumulation) across all sites pooled together
cuml.taxa.all.sites <- cuml.taxa.fun(EX = dat$comm.long)

# Examine site-level patterns of species accumulation
# First, sort the comm.dat dataframe to make sure its ordered by site
comm.dat <- dat$comm.long %>% 
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
# Note that the thick line indicates the total number of taxa among all sites
# Write species accumulation curve to pdf/tex file
pdf(file=paste('~/Google Drive/LTER Metacommunities/Manuscripts/MS3_metacom-stability-analysis/supplemental_methods_figs/',data.set,'_species_accumulation_curve.pdf',sep=''))
ggplot(data=cuml.taxa.by.site, aes(x = year, y = no.taxa)) +
  geom_point(aes(color = SITE_ID)) +
  geom_line(aes(color = SITE_ID)) +
  geom_point(data = cuml.taxa.all.sites, aes(x=year, y=no.taxa), size = 3) +
  geom_line(data = cuml.taxa.all.sites, aes(x=year, y=no.taxa), size = 1.5) +
  xlab("Year") +
  ylab("Cumulative number of taxa") +
  guides(color = guide_legend(title = "Site")) +
  ylim(c(0, max(cuml.taxa.all.sites$no.taxa))) +
  theme_bw()  +
  theme(axis.title = element_text(size=20), axis.text = element_text(size=20))
dev.off()

#make metadata table
mtdt <- list()
mtdt$dataset <- data.set
mtdt$initial.year <- min(dat$comm.wide$DATE)
mtdt$study.length <- max(dat$comm.wide$DATE) -  min(dat$comm.wide$DATE) +1
mtdt$n.years <- length(unique(dat$comm.wide$DATE))
mtdt$n.plots <- length(unique(dat$comm.wide$SITE_ID))
mtdt$n.taxa <- length(unique(dat$comm.long$VARIABLE_NAME))
mtdt$organism <- gsub(".*-(.*)\\-.*", "\\1", data.set)
mtdt <- data.frame(mtdt)
write.csv(mtdt, file = paste("~/Google Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/metadata_tables/",data.set,"_metadata.csv", sep=""), row.names=F)
 



