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

library(tidyverse)
library(vegetarian)

divpart <- function(
                    dat.in.long = d.in.long,
                    location_name = 'SITE_ID',
                    time_step_name = 'DATE',
                    taxon_name = 'VARIABLE_NAME',
                    taxon_count_name = 'VALUE',
                    ...){

    dat <- long_to_wide(dat.in.long)

    dat.list <- split(dat, dat$DATE)

    div <- plyr::ldply(dat.list, function(x){
        as.data.frame(cbind(
            alpha = vegetarian::d(x[,-c(1:2)], lev = 'alpha', q = 0), 
            beta = vegetarian::d(x[,-c(1:2)], lev = 'beta', q = 0),
            gamma = vegetarian::d(x[,-c(1:2)], lev = 'gamma', q = 0)
        ))        
    }, .id = 'year'
    )

    return(div)

}
