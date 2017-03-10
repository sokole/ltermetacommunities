

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

#######################################################
#######################################################
## -- extract 1 time step to test function
######################################################
######################################################

dat.comm <- subset(d.comm.wide, SITE_ID == 1) %>%
    select(-SITE_ID) %>% 
    data.frame(row.names = NULL) %>% 
    arrange(DATE) %>% 
    na.omit() %>%
    gather(SPP, VALUE, -DATE)

codyn::rank_shift(df = dat.comm,
                  time.var = 'DATE',
                  species.var = 'SPP',
                  abundance.var = 'VALUE',
                  replicate.var = as.character(NA))


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
    d.stats = codyn::rank_shift(
                         df = .,
                         time.var = 'DATE',
                         species.var = 'SPP',
                         abundance.var = 'VALUE',
                         replicate.var = as.character(NA)))

lapply(d.rankshift.stats.by.site, function(x) x)
 

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

