#######################################################
# check for packages
#######################################################
package.list <- c('vegan','reshape2','dplyr', 'tibble')
for(i in package.list){
  if(!i %in% row.names(installed.packages())) install.packages(i)
}

library(dplyr)

#######################################################
# -- download data off google drive using id
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
  reshape2::dcast(SITE_ID + DATE ~ VARIABLE_NAME,
                  value.var = 'VALUE',
                  fun.aggregate = mean)

#######################################################
# -- get env data, make wide
#######################################################
d.env.long <- subset(d.in.long, OBSERVATION_TYPE == 'ENV_VAR')

d.env.wide <- d.env.long %>% 
  reshape2::dcast(SITE_ID + DATE ~ VARIABLE_NAME,
                  value.var = 'VALUE',
                  fun.aggregate = mean)

#######################################################
# -- PCNM matrix from spatial coords
#######################################################
d.space.long <- subset(d.in.long, OBSERVATION_TYPE == 'SPATIAL_COORDINATE')

d.space.wide <- d.space.long %>% 
  reshape2::dcast(SITE_ID ~ VARIABLE_NAME,
                  value.var = 'VALUE',
                  fun.aggregate = mean)

d.pcnm.wide <- data.frame(
  'SITE_ID' = d.space.wide$SITE_ID,
  (d.space.wide %>%
  tibble::column_to_rownames('SITE_ID') %>% 
  as.data.frame() %>% 
  dist() %>%
  vegan::pcnm())$vectors)

#######################################################
#######################################################
## -- updated variation partitioning analysis for one timestep
######################################################
######################################################

dat.env <- subset(d.env.wide, DATE == 1) %>%
  select(-DATE)
dat.comm <- A.hell
dat.pcnm <- d.pcnm

###################