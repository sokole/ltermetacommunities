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

#read the data files
nwt.xy <- read.csv("ESA_2016/NWT_coordinates.csv")
nwt.comm.long <- read.csv("ESA_2016/NWT_plantcomp.csv")[,c(2,4,5,6)]

#reformat long-matrix to wide-matrix
nwt.comm.wide <- tidyr::spread(nwt.comm.long, 
                               USDA_code, abund,
                               fill = 0)

#check dimensions of the nwt.comm.wide matrix
dim(nwt.comm.wide)

# ---------------------------------------------------------------
# -- FUNCTIONS that work on a single long comm matrix
# ---------------------------------------------------------------

# -- make vegetarian::d compatible with long data, send it two vectors
fn.divpart.long <- function(
  site.id.vect,
  spp.vect,
  abund.vect,
  ...){
  df.long <-  data.frame(
    site.id.vect,
    spp.vect,
    abund.vect
  )
  
  df.grouped.long <- dplyr::group_by(df.long, site.id.vect, spp.vect)
  df.summarized.long <- summarise(df.grouped.long, 
                             abund.mean = mean(abund.vect))
  
  df.wide <- tidyr::spread(
    df.summarized.long$site.id.vect,
    df.summarized.long$spp.vect,
    df.summarized.long$abund.mean,
    fill = 0)[,-1]
  
  vegetarian::d(df.wide, wts = rowSums(df.wide), ...)
}

# ---------------------------------------------------------------
# apply the function to the entire data set
fn.divpart.long(
  site.id.vect = nwt.comm.long$plot, #site identifier
  spp.vect = nwt.comm.long$USDA_code, #species names
  abund.vect = nwt.comm.long$abund #abundances
)

# -- group by time
dat_diversities_by_timestep <- nwt.comm.long %>% group_by(year) %>% 
  summarise(
    alpha_q1 = fn.divpart.long(plot, USDA_code, abund, lev = 'alpha', q = 1),
    beta_q1 = fn.divpart.long(plot, USDA_code, abund, lev = 'beta', q = 1),
    gamma_q1 = fn.divpart.long(plot, USDA_code, abund, lev = 'gamma', q = 1),
    alpha_q2 = fn.divpart.long(plot, USDA_code, abund, lev = 'alpha', q = 2),
    beta_q2 = fn.divpart.long(plot, USDA_code, abund, lev = 'beta', q = 2),
    gamma_q2 = fn.divpart.long(plot, USDA_code, abund, lev = 'gamma', q = 2)
  )
