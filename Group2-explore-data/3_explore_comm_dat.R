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

for (package in c('dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom', 'ggplot2', 'iNEXT', 'grDevices', 'RColorBrewer', 'tidyverse')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}  #,'BiodiversityR'

# ---------------------------------------------------------------------------------------------------
# Assign L3 data set of interest
# NOTE: Google Drive file ID is different for each dataset

#marine
data.set <- "usvi-coral-castorani"
data.set <- "mcr-coral-castorani"
data.set <- "mcr-algae-castorani"
data.set <- "mcr-inverts-castorani"
data.set <- "mcr-fish-castorani"
data.set <- "sbc-algae-castorani"
data.set <- "sbc-fish-castorani"
data.set <- "sbc-mobileInverts-castorani"
data.set <- "sbc-sessileInverts-castorani"

#freshwater
data.set <- "ntl-zooplankton-stanleyLottig"
data.set <- "ntl-fish-stanleyLottig"
data.set <- "fce-fish-rehageWet"
data.set <- "fce-fish-rehageDry"
data.set <- "fce-algae-marazzi"
data.set <- "fce-diatoms-marazzi"

#terrestrial 
data.set <- "and-plants-mtStHelens"
data.set <- "jrn-plants-compagnoni"
data.set <- "cdr-plants-compagnoni"
data.set <- "sgs-plants-catano"
data.set <- "sev-grasshopper-compagnoni"
data.set <- "cdr-grasshopper-compagnoni"
data.set <- "knz-grasshopper-compagnoni"
data.set <- "luq-snails-compagnoni"
data.set <- "jrn-lizards-hope"
data.set <- "cap-herps-banville"
data.set <- "bes-birds-nilon"
data.set <- "and-birds-wisnoski"
data.set <- "cap-birds-banville"


#----------------------------------------------------------------------------------------------------
# IMPORT DATA
#Using Google Drive File Stream. Works the same whether streaming data or data are cached locally:
L3dat <- read.csv(paste("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-", data.set, ".csv", sep=""), stringsAsFactors=F)

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

#---------------------------------------------------------------------------------------------------
# Create color palette for heatmaps
heat.pal.spectral <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

#---------------------------------------------------------------------------------------------------
# CHECK DATA STRUCTURE AND SPATIOTEMPORAL SAMPLING EFFORT

# How are communities sampled across space and time?
# Write spatiotemporal sampling effort plot to pdf 
pdf(file=paste('MS3-Supp-Info/', data.set, '_spatiotemporal_sampling_effort.pdf',sep=''))
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

# Plot number of taxa through time
# Note that the thick line indicates the total number of taxa among all sites
#write to file:
pdf(file=paste('MS3-Supp-Info/', data.set, '_num_taxa_over_time.pdf',sep=''))
ggplot(data=no.taxa$no.taxa, aes(x=DATE, y=no.taxa)) +
  geom_point(aes(color = SITE_ID)) +
  geom_line(aes(color=SITE_ID)) +
  geom_point(data=no.taxa$total.no.taxa, aes(x=DATE, y=no.taxa), color="black", size=3) +
  geom_line(data=no.taxa$total.no.taxa, aes(x=DATE, y=no.taxa), color="black", size=1) +
  xlab("Year") +
  ylab("Number of taxa observed") +
#  guides(color = guide_legend(title = "Site")) +
  ylim(c(0, max(no.taxa$total.no.taxa$no.taxa))) +
  theme_bw() +
  theme(axis.title = element_text(size=20), axis.text = element_text(size=20), legend.position = "none")
  dev.off()
# ---------------------------------------------------------------------------------------------------
# SITE-SPECIFIC AND TOTAL SPECIES ACCUMULATION CURVES

# Make a function that returns the cumulative number of taxa observed for a given set of community data (this version cannot handle missing years)
#cuml.taxa.fun <- function(EX){
#  taxa.t.list <- list() # Make empty list
  
#  Loop over each year, creating a list that contains the unique taxa found in each year
#  for(t in 1:length(unique(EX$DATE))){
#    tmp.dat <- subset(EX, EX$DATE == t + (min(EX$DATE) - 1))
#    tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0) 
#    taxa.t.list[[t]] <- unique(tmp.dat.pres$VARIABLE_NAME)
#  }
 
 
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
  separate(rnames, c("SITE_ID", "todrop"), sep = "\\.") %>%
  select(-todrop)

# Plot the cumulative number of taxa observed at each site, as well as across all sites together
# Note that the thick line indicates the total number of taxa among all sites
# Write species accumulation curve to pdf/tex file
pdf(file=paste('MS3-Supp-Info/', data.set,'_species_accumulation_curve.pdf', sep=''))
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
  theme(axis.title = element_text(size=20), axis.text = element_text(size=20), legend.position = "none") #
dev.off()


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


#visualize spp accumulation curve over space:
no.taxa.space <- cuml.taxa.space.fun(comm.dat)

pdf(file=paste('MS3-Supp-Info/', data.set,'_species_accumulation_space.pdf', sep=''))
plot(rownames(no.taxa.space), no.taxa.space$no.taxa, pch = 19, type = "o", xaxt="n", bty="l", xlab = "Cumulative number of sites", ylab = "Cumulative number of taxa", cex=1.5, lwd=3, cex.lab=1.5)
axis(side=1, at = rownames(no.taxa.space), labels = seq(1,length(no.taxa.space$site),1))
dev.off()

# ---------------------------------------------------------------------------------------------------
# COUNT SHARED SPECIES BETWEEN EACH SITE

# Function to count species shared between sites in a site by species matrix
# Specify if you want the output as a matrix or as a dataframe in long form
shared.species <- function(comm, output = "matrix"){
  sites <- comm[,1]
  share.mat <- matrix(NA, nrow = length(sites), ncol = length(sites), dimnames = list(sites, sites))
  site.pairs <- expand.grid(site1 = sites, site2 = sites)
  for(pair in 1:nrow(site.pairs)){
    # Pull out each site combo
    site1 <- comm[site.pairs$site1[pair],][,-1]
    site2 <- comm[site.pairs$site2[pair],][,-1]
    
    # Count shared species
    if(output == "matrix"){
      share.mat[site.pairs$site1[pair],site.pairs$site2[pair]] <- sum(site1 == 1 & site2 == 1)
    }
    if(output == "dataframe"){
      site.pairs[pair,"shared"] <- sum(site1 == 1 & site2 == 1)
    }
  }
  
  if(output == "matrix") return(share.mat)
  if(output == "dataframe") return(site.pairs)
}

# Aggregate years together by looking at cumulative abundances
comm.cumul <- comm.wide %>% group_by(SITE_ID) %>% select(-OBSERVATION_TYPE, -DATE) %>% 
  summarise_all(sum)
comm.wide.pa <- cbind(comm.cumul[,1], decostand(comm.cumul[,-1], method = "pa"))
shared.species(comm.wide.pa, output = "matrix")

# Or to visualize differences
shared.taxa <- shared.species(comm.wide.pa, output = "dataframe")
pdf(file=paste('MS3-Supp-Info/', data.set,'_spp_shared.pdf', sep=''))
ggplot(shared.taxa, aes(x = site1, y = site2, fill = shared)) +
  geom_raster() +
  scale_fill_gradientn(colours = heat.pal.spectral(100), name = "Shared taxa") +
  theme_bw() +
  xlab("Site 1") +
  ylab("Site 2") +
  theme(aspect.ratio = 1, axis.text.x = element_text(angle = 90)) + 
  annotate("text", x = shared.taxa$site1, y = shared.taxa$site2, label = shared.taxa$shared)
dev.off()

#make metadata table
mtdt <- list()
mtdt$dataset <- data.set
mtdt$initial.year <- min(dat$comm.long$DATE)
mtdt$study.length <- max(dat$comm.long$DATE) -  min(dat$comm.long$DATE) +1
mtdt$n.years <- length(unique(dat$comm.long$DATE))
mtdt$n.plots <- length(unique(dat$comm.long$SITE_ID))
mtdt$n.taxa <- length(unique(dat$comm.long$VARIABLE_NAME))
mtdt$organism <- gsub(".*-(.*)\\-.*", "\\1", data.set)
#mtdt$units <-  unique(dat$comm.wide$OBSERVATION_TYPE)
mtdt <- data.frame(mtdt)
write.csv(mtdt, file = paste("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/metadata_tables/",data.set,"_metadata.csv", sep=""), row.names=F)
