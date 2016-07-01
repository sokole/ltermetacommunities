# ---------------------------- #
# ESA Workshop - 8 August 2016 #
# ---------------------------- #

# Set working environment
rm(list = ls())
setwd("~/GitHub/ltermetacommunities/")

# Check for and install required packages
for (package in c('dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# Load packages
library(dplyr)
library(tidyr)
library(vegetarian)
library(vegan)
library(metacom)

# Read in NWT plant community data and site coordinates 
nwt.xy <- read.csv("ESA_2016/NWT_coordinates.csv")
nwt.comm.long <- read.csv("ESA_2016/NWT_plantcomp.csv")[,c(2,4,5,6)]
dim(nwt.comm.long)

nwt.comm.wide <- tidyr::spread(nwt.comm.long, USDA_code, abund)

group_by(nwt.comm.wide, year)

# Function to pass long form data frames to metacom package
fn.ems.long <- function(
  site.id.vect,
  spp.vect,
  abund.vect,
  ...){
  df.wide <- tidyr::spread(
    data.frame(
      site.id.vect,
      spp.vect,
      abund.vect
  ),
  spp.vect,
  abund.vect,
  fill = 0)[,-1]

  IdentifyStructure(metacom::Metacommunity(
    comm = vegan::decostand(df.wide, method = 'pa'),
    method = 'r1', sims = 10))
}

#### We need to figure out NA handling, lots of NAs in the NWT dataset

# Create data frame of EMS for each time step
ems.by.timestep <- nwt.comm.long %>% group_by(year) %>% summarise(
  ems = fn.ems.long(plot, USDA_code, abund)
)
