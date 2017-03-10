
#######################################################
# -- download test data off google drive using google-id
#######################################################
data_id_googledrive <- "0B2P104M94skvUmtsZmxUek1lQVk" #simulated data
download.link <- paste0("https://drive.google.com/uc?export=download&id=",data_id_googledrive)
d.in.long <- read.csv(file = download.link, header = T,
                      stringsAsFactors = FALSE)

test.dat <- d.in.long
# ------------------------------------------------------------------
# Function to make rankshift work on longform
# This takes longform data, calculates global mean mean rankshift
# and also the mean of mean local rankshift across the full time
# period. 
# ------------------------------------------------------------------

fn.rankshift.long <- function(d.in.long,
                              ...){

### load required packages
    library(dplyr)
    library(tidyr)

### ------------------------------------------------------------------
### get community data, make wide
### ------------------------------------------------------------------
    d.comm.wide <- d.in.long %>%
        filter(OBSERVATION_TYPE == 'TAXON_COUNT') %>%
        group_by(SITE_ID, DATE, VARIABLE_NAME) %>%
        summarise(VALUE = mean(VALUE)) %>%
        spread(VARIABLE_NAME, VALUE) %>%
        ungroup()

    
### ------------------------------------------------------------------
### use dplyr::group_by and dplyr::do to apply by site
### ------------------------------------------------------------------
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

### calculate mean rankshift of all timesteps for each site
    mean.alpha.rankshift <- d.rankshift.stats.by.site %>%
        summarise(meanMRS = mean(MRS))
    
### calculate mean of mean rankshift of all timesteps for each site
### i.e. the mean alpha level rankshift
    mean.mean.alpha.rankshift <- mean.alpha.rankshift %>%
        summarise(mean_alpha = mean(meanMRS))

### convert site-level data to regional
### taking the MEAN of each species, not the SUM - CHECK THIS ------    
    dat.comm.by.all <- d.comm.wide %>%
        data.frame(row.names = NULL) %>% 
        arrange(DATE) %>%
        gather(SPP, VALUE, -SITE_ID, -DATE) %>%
        group_by(DATE, SPP) %>%
        summarise(VALUE = mean(VALUE)) %>%
        ungroup() 
    
### calculate gamma level rankshift
### i.e. rankshifts in regional species pool    
    d.rankshift.stats.by.all <- codyn::rank_shift(
                                           df = dat.comm.by.all,
                                           time.var = 'DATE',
                                           species.var = 'SPP',
                                           abundance.var = 'VALUE',
                                           replicate.var = as.character(NA))
    
    mean.gamma.rankshift <- d.rankshift.stats.by.all %>%
        summarise(mean_gamma = mean(MRS))

### calculate the mean of these
### i.e. the mean of all timesteps    
    rankshift.results <- cbind(mean.mean.alpha.rankshift,
                               mean.gamma.rankshift) %>%
        gather(Metric, Value)

    return(list(summary = rankshift.results,
                local.raw = d.rankshift.stats.by.site,
                local.mean = mean.alpha.rankshift,
                regional = d.rankshift.stats.by.all))
    
}

rankshift.output.test <- fn.rankshift.long(test.dat)

### write your output as a csv file in the Group 3 folder
result.file.path <-
    file.path('Group3-diversity-metrics/rankshift-results.csv')
write.csv(rankshift.output.test$summary, file = result.file.path)
