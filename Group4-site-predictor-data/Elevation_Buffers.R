library(raster)
library(sp)
library(rgdal)
library(rgeos)
setwd("~/Documents/lter_metacommunities")

#loading data
ETOPO1 <- raster("ETOPO1_Ice_g_geotiff.tif") #elevation raster
lter_sites=readOGR("lterDomains_110410.kml",layer="lterDomains_110410") #lter sites

#centroid of LTER sites
lter_centroid=gCentroid(lter_sites, byid=TRUE, id = lter_sites$Name)

#proj4string :[+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0]
#transform to m for buffers 
#cell size is about 1.83 km 
#radius - diameter, percent of cell
#10 - 20 , 20/1.83
#30 - 60 , 60/1.83
#60 - 120 , 120/1.83
#90 - 180 , 180/1.83

lter.robin <- spTransform(lter_centroid, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

#using meters for Robin Projection
lterRobin_10 <- gBuffer(lter.robin, width=20000, byid=TRUE)
lterRobin_30 <- gBuffer(lter.robin, width=60000, byid=TRUE)
lterRobin_60 <- gBuffer(lter.robin, width=120000, byid=TRUE)
lterRobin_90 <- gBuffer(lter.robin, width=180000, byid=TRUE)

lterRobin_10 <- spTransform(lterRobin_10, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
lterRobin_30 <- spTransform(lterRobin_30, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
lterRobin_60 <- spTransform(lterRobin_60, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
lterRobin_90 <- spTransform(lterRobin_90, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#cropping elevation raster to buffers and extracting elevation infromation
ElevationRaster_90km <- mask(ETOPO1, lterRobin_90)
Elevation_90km <- extract(ElevationRaster_90km, lterRobin_90) ; names(Elevation_90km) <- names(lterRobin_90)
Elevation_60km <- extract(ElevationRaster_90km, lterRobin_60) ; names(Elevation_60km) <- names(lterRobin_60)
Elevation_30km <- extract(ElevationRaster_90km, lterRobin_30) ; names(Elevation_30km) <- names(lterRobin_30)
Elevation_10km <- extract(ElevationRaster_90km, lterRobin_10) ; names(Elevation_10km) <- names(lterRobin_10)

SD_10=t(as.data.frame(lapply(Elevation_10km, sd)))
SD_30=t(as.data.frame(lapply(Elevation_30km, sd)))
SD_60=t(as.data.frame(lapply(Elevation_60km, sd)))
SD_90=t(as.data.frame(lapply(Elevation_90km, sd)))

SD_LTER=cbind(SD_10,SD_30,SD_60,SD_90) ; colnames(SD_LTER) = c("10km","30km","60km","90km")
SD_LTER
