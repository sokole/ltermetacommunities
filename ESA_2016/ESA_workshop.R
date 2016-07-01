for (package in c('dplyr', 'tidyr', 'vegetarian', 'vegan')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
library(dplyr)
library(tidyr)
library(vegetarian)
library(vegan)

nwt.xy <- read.csv("ESA_2016/NWT_coordinates.csv")
nwt.comm.long <- read.csv("ESA_2016/NWT_plantcomp.csv")[,c(2,4,5,6)]
dim(nwt.comm.long)

nwt.comm.wide <- tidyr::spread(nwt.comm.long, USDA_code, abund)

group_by(nwt.comm.wide, year)
