##########################################################################
# R Code to extract maximum distances across extents of entire LTER sites
# Sydne Record 5/17/2018
###########################################################################

#### Note shapefile sent to Sydne Record by Margaret OBrien on 5/17/2018. Margaret will send provenance for file once it is uploaded to EDI. At that point, Sydne will update this script with full details.

library(raster) 
library(rgdal)
library(sp)

# Read in shapefile of LTER site polygons using Google drive file stream
p <- shapefile("C:/Users/srecord/Dropbox/Metacommunity/lterDomains/lterDomains_110410.shp") # Margaret OBrien will send data identifier when it is ready.
p <- shapefile("~/Google Drive File Stream/My Drive/LTER Metacommunities/Site_boundaries/lterDomains/lterDomains_110410.shp", stringsAsFactors=F) # Margaret OBrien will send data identifier when it is ready.

# PUll out three letter site acronyms
site <- p$SITE
within_siteName <- p$NAME

# generate matrix of extents for each site (rows=sites, columns=)
e <- t(sapply(1:length(p), function(i) as.vector(extent(p[i,]))))

# calculate distances between latitudes and longitudes
londist <- e[,2]-e[,1]
latdist <- e[,4]-e

# Calculate summary stats of lat and lon distances
summary(londist)
summary(latdist)

# calculate max distance between points (corners of extents) using Pythagorean Theorem
pythag <- function(a,b) sqrt((a^2) + (b^2))
maxdistdeg <- pythag(a=londist, b=latdist)

# rough conversion of maximum distances from degrees to kilometers
maxdistkm <- maxdistdeg*110

# generate output matrix of max distances across extents by site
LTERextents <- cbind(site, within_siteName, maxdistdeg, maxdistkm)

# write LTERextents object to .csv file
write.csv(LTERextents, 'LTERextents.csv')
