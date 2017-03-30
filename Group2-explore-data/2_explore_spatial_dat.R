# ----------------------------------------- #
# QA/QC Coordinate Data - 30 March 2017     #
# ----------------------------------------- #

# Set working environment
rm(list = ls())

# Set working environment
setwd("~/Google Drive/LTER Metacommunities")

# Check for and install required packages
for (package in c('dplyr', 'tidyr', 'XML', 'sp', 'geosphere', 'rgdal','maps','reshape2',
                  'ggplot2', 'grDevices', 'RColorBrewer')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


###########################################
# Assign data set of interest
# NOTE: After running the '1_format_raw_data.R' script, each dataset has been stored as an .Rdata list in the "Intermediate_data" folder on Google Drive. The Google Drive file ID is different for each dataset.

# SBC LTER (Santa Barbara Coastal): Macroalgae
data.set <- "SBC-algae-Castorani_Lamy"

# SBC LTER (Santa Barbara Coastal): Sessile invertebrates
data.set <- "SBC-sessile_invert-Castorani_Lamy"

# SBC LTER (Santa Barbara Coastal): Mobile invertebrates
data.set <- "SBC-mobile_invert-Castorani_Lamy"

# SBC LTER (Santa Barbara Coastal): Fishes
data.set <- "SBC-fish-Castorani_Lamy"

# ---------------------------------------------------------------------------------------------------
# IMPORT DATA
load(paste("Intermediate_data/", data.set,".Rdata", sep=""))  # Read in .Rdata list
summary(dat)

# ---------------------------------------------------------------------------------------------------
# MAP THE SITES LOTS OF WAYS

# Create color palette for heatmaps
heat.pal.spectral <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

# Visualize the distance matrix:
dist.bt.sites <- melt(dat$distance.mat)  #windows()

# Distance matrix plot
ggplot(data = dist.bt.sites, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradientn(colours = heat.pal.spectral(100), name = "Distance") +
  xlab("Site") +
  ylab("Site")

# ---------------------------------------------------------------------------------------------------
# Plot sites in geographic space without maps
ggplot(data = dat$longlat, aes(x=longitude, y=latitude)) + 
  geom_point(fill = "blue", shape = 21, size = 3) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()

# ---------------------------------------------------------------------------------------------------
# Overlay geographic location of sites with maps
#windows() 
#quartz()
#plot on world map
map(database="world", col="gray90", fill=TRUE)
points(dat$longlat, pch=19, col="red", cex=0.5)

#change database and region to zoom into your particular study area
map(database="state", col="gray90", fill=TRUE)
points(dat$longlat, pch=19, col="red", cex=0.5)

map(database="county", region="california", # NOTE: Change this based on the state 
    col="gray90", fill=TRUE)
points(dat$longlat, pch=19, col="red", cex=0.5)

