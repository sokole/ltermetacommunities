#clear workspace
rm(list=ls())

# Set working directory to github repo
setwd("~/Documents/ltermetacommunities") 
#list all the final data files in the sites folder:
file.list <- list.files("ESA_2017/env_data_raw/sites")
#make all the vectors we are planning to fill
CV_mean <- vector(length=0)
CV_min <- vector(length=0)
CV_max <- vector(length=0)
CV_overall <- vector(length=0)
site <- vector(length=0)
SD_mean <- vector(length=0)
SD_min <- vector(length=0)
SD_max <- vector(length=0)
SD_overall <- vector(length=0)

for (file in file.list) {
#loop through all the files and calculate several CVs and SDs:
dat <- read.csv(paste("ESA_2017/env_data_raw/sites/",file,sep=""))

tmean_annual <- tapply(dat$tmean, dat$year, mean)
tmin_annual <- tapply(dat$tmean, dat$year, min)
tmax_annual <- tapply(dat$tmean, dat$year, max)

sitecode <- substr(file,1,3)
site <- append(site,sitecode)
SD_mean <- append(SD_mean, sd(tmean_annual))
CV_mean_ann <- sd(tmean_annual)/mean(tmean_annual) * 100
CV_mean <- append(CV_mean,CV_mean_ann)
CV_min_ann <- sd(tmin_annual)/mean(tmin_annual) * 100
CV_min <- append(CV_min,CV_min_ann)
SD_min <- append(SD_min, sd(tmin_annual))
CV_max_ann <- sd(tmax_annual)/mean(tmax_annual) * 100
CV_max <- append(CV_max,CV_max_ann)
SD_max <- append(SD_max, sd(tmax_annual))
CVoverall <- sd(dat$tmean)/mean(dat$tmean) * 100
CV_overall <- append(CV_overall,CVoverall)
SD_overall <- append(SD_overall, sd(dat$tmean))
}


CV_env <- as.data.frame(cbind(site,CV_mean, CV_min,CV_max,CV_overall, SD_mean, SD_min, SD_max, SD_overall))
write.csv(CV_env, "ESA_2017/final_data/CV_env.csv", row.names=F)

