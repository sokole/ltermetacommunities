# Calculating alpha, beta and gamma diversity for each year. 
#
# Uses vegetarian 'd' function. Exports a dataframe with the columns:
# year, alpha, beta and gamma. 

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
