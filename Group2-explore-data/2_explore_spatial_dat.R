# ----------------------------------------- #
# QA/QC Coordinate Data - 17 September 2016 #
# ----------------------------------------- #

# Set working environment
rm(list = ls())

# Set working environment
setwd("~/Google Drive/LTER Metacommunities")

# Check for and install required packages
for (package in c('dplyr', 'tidyr', 'XML', 'sp', 'geosphere', 'rgdal','maps','reshape2','ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


###########################################
# Assign data set of interest
# NOTE: Each dataset is a .Rdata list in the Intermediate_data folder on Google Drive. The Google Drive file ID is different for each dataset.

# SBC LTER (Santa Barbara Coastal)
data.set <- "SBC-Lamy-Castorani"
data.key <- "0B7o8j0RLpcxiTnB5a01YU2pWdk0" # Google Drive file ID 

---------------------------------------------------------------------------------------------------
# IMPORT DATA
#read in .Rdata list
#Why doesn't this work?  Help!
#load(sprintf("https://drive.google.com/open?id=", data.key)) 
load(paste("Intermediate_data/",data.set,".Rdata", sep="")) #workaround
summary(dat)

#MAP THE SITES LOTS OF WAYS
#visualize the distance matrix:
dissites=melt(dat$distancemat)
#windows()
ggplot(data = dissites, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

#windows() 
#quartz()
#plot on world map
map(database="world", col="gray90", fill=TRUE)
points(dat$longlat, pch=19, col="red", cex=0.5)

#change database and region to zoom into your particular study area
map(database="state", col="gray90", fill=TRUE)
points(dat$longlat, pch=19, col="red", cex=0.5)
map(database="county", region="california",col="gray90", fill=TRUE)
points(dat$longlat, pch=19, col="red", cex=0.5)

