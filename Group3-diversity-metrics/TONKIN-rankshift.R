

#######################################################
# check for packages
#######################################################
package.list <- c('vegan','tidyr','dplyr', 'codyn')
for(i in package.list){
  if(!i %in% row.names(installed.packages())) install.packages(i)
}

library(dplyr)
library(tidyr)


#######################################################
# -- download data off google drive using google-id
#######################################################
data_id_googledrive <- "0B2P104M94skvUmtsZmxUek1lQVk" #simulated data
download.link <- paste0("https://drive.google.com/uc?export=download&id=",data_id_googledrive)
d.in.long <- read.csv(file = download.link, header = T,
                      stringsAsFactors = FALSE)

#######################################################
# -- get community data, make wide
#######################################################
d.comm.long <- subset(d.in.long, OBSERVATION_TYPE == 'TAXON_COUNT')

d.comm.wide <- d.comm.long %>% 
    group_by(SITE_ID, DATE, VARIABLE_NAME) %>%
             summarise(VALUE = mean(VALUE)) %>%
             spread(VARIABLE_NAME, VALUE) %>%
             ungroup()





    
####################################
# use dplyr::group_by and dplyr::do to apply by site
####################################
dat.comm.by.site <- d.comm.wide %>%
    data.frame(row.names = NULL) %>% 
    arrange(DATE) %>% 
    gather(SPP, VALUE, -SITE_ID, -DATE) %>%
    group_by(SITE_ID) 

d.rankshift.stats.by.site <- do(
    .data = dat.comm.by.site,
    codyn::rank_shift(
               df = .,
               time.var = 'DATE',
               species.var = 'SPP',
               abundance.var = 'VALUE',
               replicate.var = as.character(NA)))

mean.alpha.rankshift <- d.rankshift.stats.by.site %>%
    summarise(meanMRS = mean(MRS))

mean.mean.alpha.rankshift <- mean.alpha.rankshift %>%
    summarise(mean_alpha = mean(meanMRS))
    
dat.comm.by.all <- d.comm.wide %>%
    data.frame(row.names = NULL) %>% 
    arrange(DATE) %>%
    gather(SPP, VALUE, -SITE_ID, -DATE) %>%
    group_by(DATE, SPP) %>%
    summarise(VALUE = mean(VALUE)) %>%
    ungroup() 
 
d.rankshift.stats.by.all <- codyn::rank_shift(
                         df = dat.comm.by.all,
                         time.var = 'DATE',
                         species.var = 'SPP',
                         abundance.var = 'VALUE',
                         replicate.var = as.character(NA))

mean.gamma.rankshift <- d.rankshift.stats.by.all %>%
    summarise(mean_gamma = mean(MRS))

rankshift.results <- cbind(mean.mean.alpha.rankshift, mean.gamma.rankshift) %>%
    gather(Metric, Value)


# ------------------------------------------------------------------
# Function to make rankshift work on longform
# This takes longform data, calculates global mean mean rankshift
# and also the mean of mean local rankshift across the full time
# period. 
# ------------------------------------------------------------------

fn.rankshift.long <- function(
  d.in.long,
  ...){

#######################################################
# -- get community data, make wide
#######################################################
d.comm.long <- subset(d.in.long, OBSERVATION_TYPE == 'TAXON_COUNT')

d.comm.wide <- d.comm.long %>% 
    group_by(SITE_ID, DATE, VARIABLE_NAME) %>%
             summarise(VALUE = mean(VALUE)) %>%
             spread(VARIABLE_NAME, VALUE) %>%
             ungroup()

    


