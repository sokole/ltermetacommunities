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

# ---------------------------------------------------------------
# wrapper function to calculate compositinal stability scaling
# similar to Wang and Loureau framework

library(tidyverse)

comp_stability_components <- function(
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
    do(data.frame(bd_time = bd_vegan(.[,taxon.list]))) #default for 'bd_vegan' will return result based on hellinger distance
  
  # 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
  dat.in.regional.means <- dat.in.wide.spp %>% 
    group_by_(.dots = time_step_name) %>%
    select(one_of(c(taxon.list, time_step_name))) %>%
    summarise_all(funs(mean(., na.rm = TRUE)))
  gamma_temporal_bd <- bd_vegan(dat.in.regional.means[,taxon.list])
  
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
