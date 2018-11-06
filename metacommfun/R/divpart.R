# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Requires long-form. 

library(tidyverse)
library(vegetarian)

divpart <- function(
  site.id.vect,
  spp.vect,
  abund.vect,
  ...){
  if(length(site.id.vect > 2) & (length(unique(spp.vect)) > 1)){
    df.wide <- tidyr::spread(
      data.frame(
        site.id.vect,
        spp.vect,
        abund.vect
      ),
      spp.vect,
      abund.vect,
      fill = 0)[,-1]
    
    return(vegetarian::d(df.wide, wts = rowSums(df.wide), ...))
  }else{
    return(NA)
  }
}
