# ---------------------------- #
# ESA Workshop - 8 August 2016 #
# ---------------------------- #
# Authors: 
# Eric R. Sokol (sokole@gmail.com)
# Nathan Wisnoski (wisnoski@indiana.edu)
# 
# Last modified: 29 Jul 2016

# ---------------------------------------------------------------
# Set working environment
# ---------------------------------------------------------------
rm(list = ls())
setwd("~/GitHub/ltermetacommunities/")

# ---------------------------------------------------------------
# Check for and install required packages
# ---------------------------------------------------------------
for (package in c('dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# ---------------------------------------------------------------
# -- Read in NWT plant community data and site coordinates 
# ---------------------------------------------------------------
options(stringsAsFactors = FALSE)

#read the data files
nwt.xy <- read.csv("ESA_2016/NWT_coordinates.csv")
nwt.comm.long <- read.csv("ESA_2016/NWT_plantcomp.csv")[,c(2,4,5,6)]

# ---------------------------------------------------------------
# -- FUNCTIONS that work on a single long comm matrix
# ---------------------------------------------------------------

# -- make vegetarian::d compatible with long data, send it two vectors
fn.divpart.long <- function(
  site.id.vect,
  spp.vect,
  abund.vect,
  ...){
  
  # combine vectors in a data.frame
  df.long <-  data.frame(
    site = site.id.vect,
    spp = spp.vect,
    abund = abund.vect
  )
  
  # average across replicate observations for a site -- if there are multiple observations for a site
  df.grouped.long <- dplyr::group_by(df.long, site, spp)
  df.means.long <- dplyr::summarise(df.grouped.long, 
                             abund.mean = mean(abund))
  
  # change from long format to wide format data.frame
  df.wide <- tidyr::spread(df.means.long,
    key = spp,
    value = abund.mean,
    fill = 0)[,-1]
  
  # calculate diversity metric
  vegetarian::d(df.wide, wts = rowSums(df.wide), ...)
}

# ---------------------------------------------------------------
# apply the function to the entire data set to calculate gamma diversity
total_gamma_diversity <- fn.divpart.long(
  site.id.vect = nwt.comm.long$plot, #site identifier
  spp.vect = nwt.comm.long$USDA_code, #species names
  abund.vect = nwt.comm.long$abund, #abundances
  lev = 'gamma',
  q = 0
)

print(total_gamma_diversity)

# -- apply the function to calculate gamma diversity for each year in the record
dat_diversities_by_timestep <- nwt.comm.long %>% group_by(year) %>% 
  summarise(
    gamma_q0 = fn.divpart.long(plot, USDA_code, abund, lev = 'gamma', q = 0)
  )

print(dat_diversities_by_timestep)

# -- apply the function to each year in the record, calculate alpha, beta, and gamma
dat_diversities_by_timestep <- nwt.comm.long %>% group_by(year) %>% 
  summarise(
    alpha_q0 = fn.divpart.long(plot, USDA_code, abund, lev = 'alpha', q = 0),
    beta_q0 = fn.divpart.long(plot, USDA_code, abund, lev = 'beta', q = 0),
    gamma_q0 = fn.divpart.long(plot, USDA_code, abund, lev = 'gamma', q = 0)
  )

print(dat_diversities_by_timestep)

