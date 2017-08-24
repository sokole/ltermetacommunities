########################################################################################
# Calculates compositional turnover at local (CV_alpha) and regional (CV_gamma) scales
# Calculates phi, which is scaling of variability, as CV_gamma / CV_alpha
# version 1.0.1 from ltermetacommunities group 3 directory on github
# Author: Eric Sokol (sokole@gmail.com)
# based on work in collaboration with Thomas Lamy
########################################################################################

options(stringsAsFactors = FALSE)

###################################################################################
# Notes on specific data sets
###################################################################################

# you can use i to choose the row from DATA-LIST for testing

# # test cases that I've tried and data QA notes -- 2017-07-25 (ERS)
# i <- 2 #SBC fish -- works
# i <- 14 #NWT -- works
# i <- 15 # FCE algae, dupes?
# i <- 11 # MCR-fish - has negative values for "abundance"?? -- works with filter to take out negatives

#not working:
# i <- 20 # NTL zoops - taxa have no names

#########################
# libraries
#########################
library(tidyverse)

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
  filter(LTER.site != 'EXAMPLE') %>%
  filter(nchar(google.id) > 0)

#######################################################
#######################################################
#######################################################
#######################################################
# FUNCTIONS
#######################################################

# ---------------------------------------------------------------
# function to calculate total beta diversity using functions ####
# available in the vegan package, default will use Hellinger distance

fn_bd_vegan <- function(
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
  D_lower_tri <- D_mat[lower.tri(D_mat, diag = FALSE)]
  n_obs <- nrow(D_mat)
  return(sum(D_lower_tri) / (n_obs * (n_obs -1)))
}


# ---------------------------------------------------------------
# wrapper function to calculate compositinal stability scaling
# similar to Wang and Loureau framework

fn_comp_stability_components <- function(
  dat.in.long = d.in.long,
  location_name = 'SITE_ID',
  time_step_name = 'DATE',
  taxon_name = 'VARIABLE_NAME',
  taxon_count_name = 'VALUE',
  ...){
  
  #########################
  # get species list
  taxon.list <- dat.in.long[,taxon_name] %>% as.character() %>% unique()
  
  ####################################
  # make species wide
  dat.in.wide.spp <- dat.in.long %>% 
    group_by_(.dots = c(location_name, time_step_name, taxon_name)) %>%
    select_(.dots = list(location_name, time_step_name, taxon_name, taxon_count_name)) %>% 
    summarise_all(funs( mean(as.numeric(.), na.rm = TRUE))) %>%
    tidyr::spread_(key_col = taxon_name, 
                   value_col = taxon_count_name,
                   fill = 0) %>%
    na.omit()
  
  ####################################
  # get n time steps and n locations
  n.times <- length(unique(dat.in.long[,time_step_name]))
  n.locations <- length(unique(dat.in.long[,location_name]))
  
  ################
  # -- total BD Hellinger
  ################
  # 1 -- temporal beta-div at each site
  
  alpha_temporal_bd <- dat.in.wide.spp %>% 
    group_by_(.dots = location_name) %>% 
    do(data.frame(bd_time = fn_bd_vegan(.[,taxon.list]))) #default for fn_bd_vegan will return result based on hellinger distance
  
  # 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
  dat.in.regional.means <- dat.in.wide.spp %>% 
    group_by_(.dots = time_step_name) %>%
    select(one_of(c(taxon.list, time_step_name))) %>%
    summarise_all(funs(mean(., na.rm = TRUE)))
  gamma_temporal_bd <- fn_bd_vegan(dat.in.regional.means[,taxon.list])
  
  # 3 -- 
  mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time, na.rm = TRUE)
  
  # 4 -- 
  phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd
  
  dat.bd.total <- data.frame(
    beta_type = 'total',
    gamma_temporal_bd,
    mean_alpha_temporal_bd,
    phi_bd,
    n_locations = n.locations,
    n_times = n.times)
  
  return(dat.bd.total)
} # End wrapper function
#######################################################
#######################################################
#######################################################
#######################################################

data_ALL <- data.frame()
for(i in 1:nrow(data_list)){
  try_result <- try({
    
    # get record
    i_data_record <- data_list[i,]
    
    # get google id used to download data
    data_id_googledrive <- i_data_record$google.id
    
    # link to read in spreadsheet from google drive
    download.link <- paste0("https://drive.google.com/uc?export=download&id=",
                            data_id_googledrive)
    
    # blank data frame
    d.in.long <- data.frame()
    
    # read data into data.frame and filter out non-taxon data, spp names that are NAs, and negative VALUES
    # make SITE_ID and DATE chars
    d.in.long <- read.csv(file = download.link, header = T,
                          stringsAsFactors = FALSE) %>%
      filter(OBSERVATION_TYPE == 'TAXON_COUNT') %>% 
      filter(!is.na(VARIABLE_NAME)) %>%
      filter(VALUE >= 0) %>%
      mutate(SITE_ID = as.character(SITE_ID),
             DATE = as.character(DATE))
    
    if('TREATMENT' %in% names(d.in.long)){
      d.in.long <- d.in.long %>%
        filter(TREATMENT %in% c(NA, 'NA', '', 'control', 'Control','CONTROL'))
    }
     
    key_list <- with(d.in.long, 
                     paste(
                       OBSERVATION_TYPE,
                       SITE_ID,
                       DATE,
                       VARIABLE_NAME,
                       VALUE,
                       sep = '_'
                     ))
    
    # get rid of exact dupes
    rows2check <- which(duplicated(key_list))
    if(length(rows2check) > 0){
      d.in.long <- data.frame(d.in.long[-rows2check,], row.names = NULL)
    }
    
    # take mean of replicate observations that are not exact dupes
    d.in.long <- d.in.long %>% group_by(
      OBSERVATION_TYPE, SITE_ID, DATE, 
      VARIABLE_NAME, VARIABLE_UNITS) %>%
      summarise(VALUE = mean(VALUE)) %>% data.frame()

    # Send data to function to calclulate compositional variability and scaling of variabilityd.bd <- data.frame()
    if(nrow(d.in.long) > 0){
      d.bd <- data.frame(
        i_data_record[,c('data.set','LTER.site','google.id','organism','body.size','dispersal.type','trophic.group','biome')],
        fn_comp_stability_components(d.in.long,
                                     location_name = 'SITE_ID',
                                     time_step_name = 'DATE',
                                     taxon_name = 'VARIABLE_NAME',
                                     taxon_count_name = 'VALUE'))
      data_ALL <- rbind(
        data_ALL,
        d.bd)
    }
   
  })
  print(i_data_record)
}

write.csv(data_ALL, 'Group3-diversity-metrics/data-comp-stability-components-v-1-0-1.csv',
          row.names = FALSE)
  


