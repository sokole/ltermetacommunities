# ---------------------------------------- #
# Qaqc Coordinate Data - 17 September 2016 #
# ---------------------------------------- #

# Set working environment
rm(list = ls())
#setwd("pathname")

# Check for and install required packages

for (package in c('dplyr', 'tidyr', 'XML', 'sp', 'geosphere', 'rgdal','maps','reshape2','ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#read in csv
id.long.dat <- "0BxUZSA1Gn1HZYTVfd2FZTWhWbm8"
data.name <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.long.dat))

#check data format
dim(data.name) 
head(data.name) 
str(data.name)

#pull out coordinate data and make sure that it is numeric
cord.data =filter(data.name, OBSERVATION_TYPE=="SPATIAL_COORDINATE");head(cord.data)
cord.data$SITE_ID=toupper(cord.data$SITE_ID)
cord.wide = spread(cord.data,VARIABLE_NAME, VALUE);head(cord.wide) #create rows from lat long
sites=c(unique(cord.wide$SITE_ID));sites

# keep the records that are _not_ duplicated
cord.wide =subset(cord.wide, !duplicated(SITE_ID));dim(cord.wide)  # here we selcet rows (1st dimension) that are different from the object dups2 (duplicated records)
cord.wide
cord.wide$latitude <- as.numeric(as.character(cord.wide$LAT)) #cord.wide$latitude <- as.numeric(as.character(cord.wide$latitude))
cord.wide$longitude <- as.numeric(as.character(cord.wide$LONG)) #cord.wide$longitude <- as.numeric(as.character(cord.wide$longitude))
cord.wide=cord.wide[c("longitude", "latitude")] #pull last two columns and reorder

#make data spatially explicit
coordinates(cord.wide) = c("longitude", "latitude") #coordinates(cord.wide) <- c("longitude", "latitude") 
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(cord.wide) <- crs.geo  # define projection system of our data to WGS84 (CHECK TO SEE IF THIS WORKS IF SPATIAL COORDINATE IS NOT IN DEC.DEGREES)
summary(cord.wide) 

#if DATA IS IN UTM OR OTHER KNOWN COORDINATE SYSTEM YOU CAN TRANSFORM IT, EG... UTM data for PHX
#coordinates(cord.wide) <- c("X", "Y")
#crs.geo = CRS("+proj=utm +zone=12 ellps=WGS84")  # define coordinate reference system of your specific data
#proj4string(cord.wide) = crs.geo # define coordinate reference system of your specific data
#summary(cord.wide) 
#cord.wide <- spTransform(cord.wide, CRS("+proj=longlat")) #transform to latlog CRS
#summary(cord.wide) #check transformation

#create a distance matrix between sites, best fit distance function TBD
distancemat=(distm(cord.wide, fun=distVincentyEllipsoid)/1000);distancemat #km distance
rownames(distancemat) = sites
colnames(distancemat) = sites
dissites=melt(distancemat)
windows()
ggplot(data = dissites, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

#plot data on map
windows() 
quartz()

#plot on world map
map(database="world", col="gray90", fill=TRUE)
points(cord.wide, pch=19, col="red", cex=0.5)

#change database and region to zoom into your particular study area
map(database="state", col="gray90", fill=TRUE)
points(cord.wide, pch=19, col="red", cex=0.5)
map(database="county", region="california",col="gray90", fill=TRUE)
points(cord.wide, pch=19, col="red", cex=0.5)

