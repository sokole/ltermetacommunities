#######################################################
# check for packages
#######################################################
package.list <- c('vegan','reshape2','dplyr', 'tibble','vegetarian')
for(i in package.list){
  if(!i %in% row.names(installed.packages())) install.packages(i)
}

library(dplyr)

#######################################################
# -- download data off google drive using google-id
#######################################################
data_id_googledrive <- "0B2P104M94skvQVprSnBsYjRzVms" #NWT plants
download.link <- paste0("https://drive.google.com/uc?export=download&id=",data_id_googledrive)
d.in.long <- read.csv(file = download.link, header = T,
                      stringsAsFactors = FALSE)

#######################################################
# -- get community data, make wide
#######################################################
d.comm.long <- subset(d.in.long, OBSERVATION_TYPE == 'TAXON_COUNT')

d.comm.wide <- d.comm.long %>% 
  reshape2::dcast(SITE_ID + DATE ~ VARIABLE_NAME,
                  value.var = 'VALUE',
                  fun.aggregate = mean)

#######################################################
# -- diverssity partitioning example, one year
#######################################################
d.comm.1time <- subset(d.comm.wide, DATE == '2014') %>%
  select(-SITE_ID, -DATE)

diversity.alpha <- vegetarian::d(d.comm.1time, lev = 'alpha', q = 1)

#######################################################
# -- diverssity partitioning example, time series
#######################################################

# ---------------------------------------------------------------
# -- FUNCTIONS that work on a single long comm matrix
# ---------------------------------------------------------------
# -- make vegetarian::d compatible with long data, send it two vectors
fn.divpart.long <- function(
  site.id.vect,
  spp.vect,
  abund.vect,
  ...){
  df.wide <- tidyr::spread(
    data.frame(
      site.id.vect,
      spp.vect,
      abund.vect
    ),
    spp.vect,
    abund.vect,
    fill = 0)[,-1]
  
  vegetarian::d(df.wide, wts = rowSums(df.wide), ...)
}

# fn.divpart.long(d.1.long$spp, d.1.long$count, lev='gamma', q=1)
# ---------------------------------------------------------------

# -- group by time

# dat_diversities_by_timestep is the data table with results
# each row in the code below calculates the metric for a column in your results table
dat_diversities_by_timestep <- d.comm.long %>% group_by(DATE) %>% 
  summarise(
    alpha_q1 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'alpha', q = 1),
    beta_q1 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'beta', q = 1),
    gamma_q1 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'gamma', q = 1),
    alpha_q2 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'alpha', q = 2),
    beta_q2 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'beta', q = 2),
    gamma_q2 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'gamma', q = 2)
  )

# write your output as a csv file in the Group 3 folder
result.file.path <- file.path('Group3-diversity-metrics/dat_diversities_by_timestep.csv')
write.csv(dat_diversities_by_timestep, file = result.file.path)
