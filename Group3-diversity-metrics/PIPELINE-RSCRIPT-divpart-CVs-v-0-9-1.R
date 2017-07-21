#######################################################
# check for packages
#######################################################
package.list <- c('vegan','reshape2','dplyr', 'tibble','vegetarian')
for(i in package.list){
  if(!i %in% row.names(installed.packages())) install.packages(i)
}

library(dplyr)

#######################################################
# -- download list of data sets off google drive using google-id
#######################################################

# google id for L3-DATA-list google sheet
id_google_data_list <- '17IKwyA1zniMP15kM8hMe_zCuYfML74_7l2io7FhaiBo'

# download L3-DATA-list as a .csv
download.link <- paste0("https://docs.google.com/spreadsheets/export?id=",
                        id_google_data_list,
                        "&format=csv")
data_list <- read.csv(file = download.link, 
                      header = T,
                      stringsAsFactors = FALSE) 

#find google.id column, and rename 'google.id' 
col_names <- names(data_list)
col_names[grep('google.id',col_names)] <- 'google.id'
names(data_list) <- col_names

#only keep necessary columns
data_list <- data_list %>% 
  select(LTER.site, organism, google.id) %>%
  filter(LTER.site != 'EXAMPLE') %>%
  filter(nchar(google.id) > 0)

#######################################################
#######################################################
#######################################################
#######################################################

#######################################################
# -- download data from google drive using google-id 
# and make massive long-form data frame
#######################################################

data_ALL_long <- data.frame()
for(i in 1:nrow(data_list)){
# for(i in 1:3){
  try({
    i_data_record <- data_list[i,]
    data_id_googledrive <- i_data_record$google.id
    
    download.link <- paste0("https://drive.google.com/uc?export=download&id=",
                            data_id_googledrive)
    
    d.in.long <- read.csv(file = download.link, header = T,
                          stringsAsFactors = FALSE)
    
    if(!'TAXON_GROUP'%in%names(d.in.long)){
      d.in.long$TAXON_GROUP <- i_data_record$organism
    }
    
    if(sum(grepl('/',d.in.long$DATE)) > 0){
      YEAR <- d.in.long$DATE %>% as.character() %>%
        as.Date(format = '%m/%d/%y') %>%
        format('%Y')
      d.in.long$DATE <- YEAR
      d.in.long$VALUE <- as.numeric(d.in.long$VALUE)
      d.in.long <- d.in.long %>%
        group_by(OBSERVATION_TYPE, SITE_ID, DATE,
                 VARIABLE_NAME, VARIABLE_UNITS, TAXON_GROUP) %>%
        summarize(VALUE = mean(VALUE, na.rm = TRUE))
    }
    
    d.temp.long <- data.frame(
      LTER.site = i_data_record$LTER.site,
      google.id = i_data_record$google.id,
      d.in.long[,c('OBSERVATION_TYPE',
                   'SITE_ID',
                   'DATE',
                   'VARIABLE_NAME',
                   'TAXON_GROUP',
                   'VALUE')]
    )
    
    data_ALL_long <- rbind(data_ALL_long,
                           d.temp.long)
  })
  print(i_data_record)
}



#######################################################
# -- get community data, make wide
#######################################################
d.comm.long <- data_ALL_long %>%
  as.data.frame() %>%
  filter(OBSERVATION_TYPE == 'TAXON_COUNT')

#######################################################
# -- get community data, make wide
#######################################################
d.env.long <- data_ALL_long %>%
  as.data.frame() %>%
  filter(OBSERVATION_TYPE == 'ENV_VAR') 

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

fn.cv <- function(x){
  x <- as.numeric(x)
  sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)
}

# fn.divpart.long(d.1.long$spp, d.1.long$count, lev='gamma', q=1)
# ---------------------------------------------------------------

# -- group by time

# dat_diversities_by_timestep is the data table with results
# each row in the code below calculates the metric for a column in your results table
dat_diversities_by_timestep <- d.comm.long %>% 
  na.omit() %>%
  group_by(LTER.site, google.id, TAXON_GROUP, DATE) %>% 
  summarise(
    alpha_q0 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'alpha', q = 0),
    beta_q0 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'beta', q = 0),
    gamma_q0 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'gamma', q = 0),
    alpha_q2 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'alpha', q = 2),
    beta_q2 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'beta', q = 2),
    gamma_q2 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'gamma', q = 2)
  ) %>%
  na.omit()


dat_div_CV <- dat_diversities_by_timestep %>% as.data.frame() %>%
  na.omit() %>%
  group_by(LTER.site, google.id, TAXON_GROUP) %>%
  summarize(CV_alpha_0 = fn.cv(alpha_q0),
            CV_beta_0 = fn.cv(beta_q0),
            CV_gamma_0 = fn.cv(gamma_q0),
            CV_alpha_2 = fn.cv(alpha_q2),
            CV_beta_2 = fn.cv(beta_q2),
            CV_gamma_2 = fn.cv(gamma_q2))

dat_div_CV_long <- dat_div_CV %>% reshape2::melt(
  id.vars = c('LTER.site','google.id','TAXON_GROUP')
)


dat_env_CV_alpha_mean <- d.env.long %>%
  filter(!is.na(VALUE)) %>% 
  group_by(LTER.site, google.id, SITE_ID, VARIABLE_NAME) %>%
  summarize(CV_env_alpha = fn.cv(VALUE)) %>% as.data.frame() %>%
  na.omit() %>%
  group_by(LTER.site, google.id, VARIABLE_NAME) %>%
  summarize(CV_env = mean(CV_env_alpha)) %>% as.data.frame()


dat_env_CV_gamma <- d.env.long %>%
  filter(!is.na(VALUE)) %>% 
  group_by(LTER.site, google.id, DATE, VARIABLE_NAME) %>%
  summarize(VALUE.mean = mean(VALUE, na.rm = TRUE)) %>% as.data.frame() %>%
  na.omit() %>%
  group_by(LTER.site, google.id, VARIABLE_NAME) %>%
  summarize(CV_env = fn.cv(VALUE.mean)) %>% as.data.frame()

d_env_CVs <- rbind(
  data.frame(dat_env_CV_alpha_mean, CV_env_scale = 'alpha_mean'),
  data.frame(dat_env_CV_gamma, CV_env_scale = 'gamma'))

d_CVs <- full_join(
  d_env_CVs,
  dat_div_CV_long
)

library(ggplot2)
d.plot <- d_CVs %>% 
  filter(variable%in%c('CV_alpha_0','CV_beta_0','CV_gamma_0')) %>%
  na.omit()

ggplot(d.plot,
       aes(CV_env,
           value,
           color = TAXON_GROUP)
       ) +
  geom_text(aes(label = LTER.site),
            size = 4) +
  facet_grid(variable ~ CV_env_scale, 
             scales = 'free')
  
# write your output as a csv file in the Group 3 folder
result.file.path <- file.path('Group3-diversity-metrics/dat_CVs_divpart_env_v-0-9-1.csv')
write.csv(dat_diversities_by_timestep, file = result.file.path)
