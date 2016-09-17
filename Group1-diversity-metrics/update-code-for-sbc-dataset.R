# Updating previous functions to work with SBC dataset

# Read data from Google Drive using tips from this page: 
# http://www.labnol.org/internet/direct-links-for-google-drive/28356/
# Get id number from sharing link and use to create download link

rm(list=ls())

library(tidyr)
library(dplyr)
library(vegan)
library(metacom)

download.link <- "https://drive.google.com/uc?export=download&id=0BxUZSA1Gn1HZYTVfd2FZTWhWbm8"
sbc.data <- read.csv(file = download.link, header = T)


# Function to analyze time series 
fn.mc.loop <- function(mc.data = mc.data, output = output){
  
  # Create the spatial dataset
  mc.spatial.long <- mc.data[which(mc.data$OBSERVATION_TYPE == "SPATIAL_COORDINATE"),
                               which(colnames(mc.data) %in% c("SITE_ID", "VARIABLE_NAME", "VALUE"))]
  mc.spatial.wide <- arrange(tidyr::spread(mc.spatial.long, key = VARIABLE_NAME, value = VALUE), SITE_ID)
  
  # Create the environmental dataset
  mc.env.long <- mc.data[which(mc.data$OBSERVATION_TYPE == "ENV_VAR"),
                           which(colnames(mc.data) %in% 
                                   c("SITE_ID", "DATE",
                                     "VARIABLE_NAME", "VALUE"))]
  
  mc.env.wide <- arrange(tidyr::spread(mc.env.long, key = VARIABLE_NAME, value = VALUE), SITE_ID)
  
  # Create the species matrix
  mc.species.long <- mc.data[which(mc.data$OBSERVATION_TYPE == "TAXON_COUNT"),
                               which(colnames(mc.data) %in% 
                                       c("SITE_ID", "DATE",
                                         "VARIABLE_NAME", "VALUE"))]
  mc.species.wide <- tidyr::spread(mc.species.long, key = VARIABLE_NAME, value = VALUE, fill = 0)
  mc.species.wide <- arrange(mc.species.wide[,which(colnames(mc.species.wide) != "")], SITE_ID) # removing potential blank cols
  
  # Loop through years to analyze time series 
  
  for(date.i in unique(mc.species.wide$DATE)){
    comm.date <- filter(mc.species.wide, DATE == date.i)[,-c(1,2)] # remove plot & year cols
    env.date <- na.omit(filter(mc.env.wide, DATE == date.i))
    spatial <- filter(mc.spatial.wide, SITE_ID %in% unique(mc.species.wide$SITE_ID))
    
    # Diversity Partitioning
    comm.date.0.a <- vegetarian::d(comm.date, lev = "alpha", q = 0)
    comm.date.0.b <- vegetarian::d(comm.date, lev = "beta", q = 0)
    comm.date.0.g <- vegetarian::d(comm.date, lev = "gamma", q = 0)
  
    comm.date.1.a <- vegetarian::d(comm.date, lev = "alpha", q = 1)
    comm.date.1.b <- vegetarian::d(comm.date, lev = "beta", q = 1)
    comm.date.1.g <- vegetarian::d(comm.date, lev = "gamma", q = 1)
  
    comm.date.2.a <- vegetarian::d(comm.date, lev = "alpha", q = 2)
    comm.date.2.b <- vegetarian::d(comm.date, lev = "beta", q = 2)
    comm.date.2.g <- vegetarian::d(comm.date, lev = "gamma", q = 2)
  
    # Variation Partitioning
    
    # have to make matrices same dimension, so removing NAs and sites without env, spa, and spe
    comm.date <- filter(mc.species.wide, DATE == date.i)
    common.sites <- dplyr::intersect(dplyr::intersect(comm.date[,1], env.date[,1]), spatial[,1])
    comm.date <- as.matrix(filter(comm.date, SITE_ID %in% common.sites)[,-c(1:2)])
    env.date <- as.matrix(filter(env.date, SITE_ID %in% common.sites)[,-c(1:2)])
    spatial <- as.matrix(filter(spatial, SITE_ID %in% common.sites)[,-1])
    
    
    comm.date.hel <- decostand(comm.date, method = "hellinger")
    comm.date.varpart <- vegan::varpart(comm.date.hel, env.date, spatial)
    vp <- vector(length = 4)
    vp <- comm.date.varpart$part$indfract$Adj.R.squared
    vp.a <- vp[1]
    vp.b <- vp[2]
    vp.c <- vp[3]
    vp.d <- vp[4]
  
    # EMS
    comm.date <- filter(mc.species.wide, DATE == date.i)[,-c(1,2)]
    comm.date.pa <- decostand(comm.date, method = "pa")
    comm.date.pa <- comm.date.pa[,which(colSums(comm.date.pa) > 0)] # remove empty cols and rows
    comm.date.pa <- comm.date.pa[which(rowSums(comm.date.pa) > 0),]
    ems.date <- Metacommunity(
      comm.date.pa,
      method = "r1", sims = 10)
    comm.date.ems.struc <- (IdentifyStructure(ems.date))  # prints the structure of the MC
  
    # Write Output
    site.output <- c(date.i,
                     comm.date.0.a, comm.date.0.b, comm.date.0.g,
                     comm.date.1.a, comm.date.1.b, comm.date.1.g,
                     comm.date.2.a, comm.date.2.b, comm.date.2.g,
                     vp.a, vp.b, vp.c, vp.d, comm.date.ems.struc)
    # print(site.output)
    output[which(rownames(output) == date.i), ] <- site.output
  }

return(output)
}

# Create summary data frame
mc.time.series <- data.frame(date = unique(na.omit(sbc.data$DATE)),
                              d.0.a = NA, d.0.b = NA, d.0.g = NA,
                              d.1.a = NA, d.1.b = NA, d.1.g = NA,
                              d.2.a = NA, d.2.b = NA, d.2.g = NA,
                              vp.a = NA, vp.b = NA, vp.c = NA, vp.d = NA,
                              ems.struc = NA)

mc.time.series.summary <- fn.mc.loop(mc.data = sbc.data, output = mc.time.series)
print(mc.time.series.summary)
