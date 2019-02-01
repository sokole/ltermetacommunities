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


# ---------------------------------------------------------------
# function to calculate total beta diversity using functions ####
# available in the vegan package, default will use Hellinger distance

library(vegan)

bd_vegan <- function(
  Y, #site by species counts or RAs
  decostand_transform = TRUE,
  decostand_method = 'hellinger', #defaults will return Hellinger distance matrix
  vegdist_method = 'euclidean', #defaults will return Hellinger distance matrix
  ...
){
  Y <- as.data.frame(Y)
  
  # exception handling for decostand_transform
  if(is.null(decostand_transform)) decostand_transform <- FALSE
  if(decostand_transform %in% c(NA, NaN)) decostand_transform <- FALSE
  if(decostand_transform != TRUE) decostand_transform <- FALSE
  
  # use decostand to transform if optiong chosen
  if(decostand_transform) Y <- vegan::decostand(Y, method = decostand_method)
  
  D <- vegan::vegdist(Y, method = vegdist_method, ...)
  D_mat <- as.matrix(D)
  # D_lower_tri <- D_mat[lower.tri(D_mat, diag = FALSE)]
  n_obs <- nrow(D_mat)
  # return(sum(D_lower_tri) / (n_obs * (n_obs -1)))
  return(sum(D_mat^2) / (n_obs * (n_obs -1)))
  
}
