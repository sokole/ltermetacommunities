# ---------------------------------------- #
# Qaqc Coordinate Data - 17 September 2016 #
# ---------------------------------------- #

# Set working environment
rm(list = ls())
#setwd("pathname")

# Check for and install required packages
for (package in c('dplyr', 'tidyr', 'rJava', 'XML', 'sp', 'raster', 'rgdal','maps')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#read in csv
data.name <- read.csv("sbc_long_dat.csv") #currently is pulling from working directory, but check change to pull from google doc

#check data format
dim(data.name) 
head(data.name) 
str(data.name)

#pull out coordinate data and make sure that it is numeric
cord.data =filter(data.name, OBSERVATION_TYPE=="SPATIAL_COORDINATE");head(cord.data)
#cord.data$X=NULL #if you have some index that seperates the sites others then site_id

cord.wide = spread(cord.data,VARIABLE_NAME, VALUE);head(cord.wide)
cord.wide$latitude <- as.numeric(as.character(cord.wide$LAT)) #cord.wide$latitude <- as.numeric(as.character(cord.wide$latitude))
cord.wide$longitude <- as.numeric(as.character(cord.wide$LONG)) #cord.wide$longitude <- as.numeric(as.character(cord.wide$longitude))
cord.wide=cord.wide[,6:7] #pull last two columns
cord.wide = cord.wide [c("LONG", "LAT")] #cord.wide = cord.wide [c("longitude", "latitude")]

#make data spatially explicit
coordinates(cord.wide) <- c("LONG", "LAT") #coordinates(cord.wide) <- c("longitude", "latitude") 

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(cord.wide) <- crs.geo  # define projection system of our data to WGS84 (CHECK TO SEE IF THIS WORKS IF SPATIAL COORDINATE IS NOT IN DEC.DEGREES)
summary(cord.wide) 

#plot data on map
windows() #quartz

#plot on world map
map(database="world", col="gray90", fill=TRUE)
points(cord.wide, pch=19, col="red", cex=0.5)

#change database and region to zoom into your particular study area
map(database="state", col="gray90", fill=TRUE)
points(cord.wide, pch=19, col="red", cex=0.5)
map(database="county", region="california",col="gray90", fill=TRUE)
points(cord.wide, pch=19, col="red", cex=0.5)

