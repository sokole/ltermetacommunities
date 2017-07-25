#######################################################
# set options
#######################################################

options(stringsAsFactors = FALSE)

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

    d.in.long <- data.frame()
    d.in.long <- read.csv(file = download.link, header = T,
                          stringsAsFactors = FALSE) %>% 
      filter(OBSERVATION_TYPE == 'TAXON_COUNT') %>%
      filter(!is.na(VARIABLE_NAME)) %>%
      filter(VALUE >= 0) 
    
    #go to next i if no data
    if(!nrow(d.in.long) > 0) next
      
    d.in.long$SITE_ID <- as.character(d.in.long$SITE_ID)
    
    if(!'TAXON_GROUP'%in%names(d.in.long)){
      d.in.long$TAXON_GROUP <- i_data_record$organism
    }
    
    # handle dates that are not years
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
      d.in.long
    )
    
    d.temp.long$DATE <- as.character(d.temp.long$DATE)
    
    data_ALL_long <- dplyr::bind_rows(data_ALL_long,
                           d.temp.long)
  })
  print(i_data_record)
}

data_CLEANED_long <- data_ALL_long 
if('TREATMENT' %in% names(data_CLEANED_long)){
  data_CLEANED_long <- data_CLEANED_long %>%
    filter(TREATMENT %in% c(NA, 'NA', '', 'control', 'Control','CONTROL'))
}

# unique(data_CLEANED_long$LTER.site)
# sum(is.na(data_CLEANED_long$VARIABLE_NAME))

key_list <- with(data_CLEANED_long, 
                 paste(
                   LTER.site,
                   google.id,
                   SITE_ID,
                   DATE,
                   TAXON_GROUP,
                   VARIABLE_NAME,
                   VALUE,
                   sep = '_'
                 ))

# get rid of exact dupes
rows2check <- which(duplicated(key_list))
data_CLEANED_long <- data.frame(data_CLEANED_long[-rows2check,], row.names = NULL)

# take mean of replicate observations that are not exact dupes
data_CLEANED_long <- data_CLEANED_long %>% group_by(
  LTER.site, google.id, OBSERVATION_TYPE, SITE_ID, DATE, 
  VARIABLE_NAME, VARIABLE_UNITS, TAXON_GROUP, TREATMENT) %>%
  summarise(VALUE = mean(VALUE))

d.comm.long <- data_CLEANED_long %>%
  as.data.frame()

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
  group_by(LTER.site, google.id, TAXON_GROUP, DATE) %>% 
  summarise(
    alpha_q0 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'alpha', q = 0),
    beta_q0 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'beta', q = 0),
    gamma_q0 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'gamma', q = 0),
    alpha_q2 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'alpha', q = 2),
    beta_q2 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'beta', q = 2),
    gamma_q2 = fn.divpart.long(SITE_ID, VARIABLE_NAME, VALUE, lev = 'gamma', q = 2)
  ) %>% na.omit()


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

# write your output as a csv file in the Group 3 folder
result.file.path <- file.path('Group3-diversity-metrics/dat_CVs_divpart_env_v-0-9-1.csv')
write.csv(dat_div_CV, file = result.file.path, row.names = FALSE)
