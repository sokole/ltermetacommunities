###########################################
# Sea surface temperature 
# Data Source: NOAA NCEP EMC CMB GLOBAL Reyn_SmithOIv2 monthly sst: Sea Surface Temperature data from Nov 1981 to Jun 2017
# http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CMB/.GLOBAL/.Reyn_SmithOIv2/.monthly/.sst/DATA/2/STEP/index.html

# Set working directory to github repo
setwd("~/Documents/ltermetacommunities") 

# load relevant libraries
library(ncdf4)
library(raster)
library(rgdal)

# Read in netcdf file of sea surface temperature
sstnc <- nc_open("~/Google Drive/LTER Metacommunities/ESA-2017/data.nc")

# Print information about the netcdf file
print(sstnc)
str(sstnc)
# Create a raster brick from the netcdf file
sst_brick <- brick("~/Google Drive/LTER Metacommunities/ESA-2017/data.nc", varname="sst")

#SBC
# Select months from Jan 2001 - Dec2016
sst_brick0116 <- subset(sst_brick, 242:421)

lats <- 34.41
longs <- 119.84
crds <- as.data.frame(cbind(longs, lats))
sst <- extract(sst_brick0116, crds)
attributes(sst) <- NULL
str(sst)
plot(sst, type="l")
tmean <- sst #for later, to match PRISM data
months <- c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month <- rep(months, 15)
year <- vector(length=0)
for(i in 1:15) {
	yrs <- rep(2000+i,12)
	year <- append(year,yrs)
}
SBC_env <- as.data.frame(cbind(year, month, tmean))
write.csv(SBC_env, file="ESA_2017/env_data_raw/sites/SBC_env.csv", row.names=F)

#USVI
# Select months from Jan1992 - Dec2015
sst_brick9215 <- subset(sst_brick, 122:409)
lats <- 18.31
longs <- 64.72
crds <- as.data.frame(cbind(longs, lats))
sst <- extract(sst_brick9215, crds)
attributes(sst) <- NULL
str(sst)
plot(sst, type="l")
tmean <- sst #for later, to match PRISM data
months <- c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month <- rep(months, 24)
year <- vector(length=0)
for(i in 1:24) {
	yrs <- rep(1991+i,12)
	year <- append(year,yrs)
}
USVI_env <- as.data.frame(cbind(year, month, tmean))
write.csv(USVI_env, file="ESA_2017/env_data_raw/sites/USVI_env.csv", row.names=F)

#MCR
# Select months from Jan2006 - Dec2015
sst_brick0615 <- subset(sst_brick, 290:409)
lats <- -17.49
longs <- 149.83
crds <- as.data.frame(cbind(longs, lats))
sst <- extract(sst_brick0615, crds)
attributes(sst) <- NULL
str(sst)
plot(sst, type="l")
tmean <- sst #for later, to match PRISM data
months <- c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month <- rep(months, 10)
year <- vector(length=0)
for(i in 1:10) {
	yrs <- rep(2005+i,12)
	year <- append(year,yrs)
}
MCR_env <- as.data.frame(cbind(year, month, tmean))
write.csv(MCR_env, file="ESA_2017/env_data_raw/sites/MCR_env.csv", row.names=F)


########################################################
# put the PRISM data into the same format in the 'sites' directory

months <- c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

file.list <- c("FCE.csv", "JRN.csv", "NTL.csv","NWT.csv")
for (file in file.list) {
#file <- file.list[1]
dat <- read.csv(paste("ESA_2017/env_data_raw/",file, sep=""), skip=10)
str(dat)
year <- substr(dat$Date,1,4)
month <- rep(months, length(unique(year)))
dat.out <- as.data.frame(cbind(year,month,dat$tmean..degrees.C.))
names(dat.out) <- c("year","month","tmean")
dat.out$year <- as.numeric(as.character(dat.out$year))
dat.out$tmean <- as.numeric(as.character(dat.out$tmean))
str(dat.out)
write.csv(dat.out,paste("ESA_2017/env_data_raw/sites/",substr(file,1,3),"_env.csv",sep=""),row.names=F)
}