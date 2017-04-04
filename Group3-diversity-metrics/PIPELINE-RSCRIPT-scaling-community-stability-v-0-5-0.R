#########################
# libraries
#########################
library(tidyverse)

source("Group3-diversity-metrics/FUNCTIONS-metacomm-stability-decomp-20170309.R")
library(ade4)
library(vegan)



#########################
#########################
#########################
#########################
# wrapper function to calculate compositinal stability components
#########################
fn_comp_stability_components <- function(
  # dat.in.long <- read.csv('Group3-diversity-metrics/TEST-DATA-Y1-long.csv')
  dat.in.long = d.in.long,
  location_name = 'SITE_ID',
  time_step_name = 'DATE',
  taxon_name = 'VARIABLE_NAME',
  taxon_count_name = 'VALUE',
  ...){
  
  #########################
  # get species list
  taxon.list <- dat.in.long[,taxon_name] %>% as.character() %>% unique()
  
  #########################
  # make species wide
  dat.in.long <- dat.in.long[,c(location_name,
                                 time_step_name,
                                 taxon_name,
                                 taxon_count_name)]
  
  dat.in.long[,taxon_count_name] <- as.numeric(dat.in.long[,taxon_count_name])
  
  dat.in.wide.spp <- dat.in.long %>% 
    group_by_(.dots = c(location_name, time_step_name, taxon_name)) %>%
    summarise_all(funs( mean(as.numeric(.), na.rm = TRUE))) %>%
    tidyr::spread_(key_col = taxon_name, 
                   value_col = taxon_count_name,
                   fill = 0) %>%
    na.omit()

  
  ######################################################
  # steps to calculate stability
  
  ################
  # -- total BD
  ################
  # 1 -- temporal beta-div at each site
  
  alpha_temporal_bd <- dat.in.wide.spp %>% 
    group_by_(.dots = location_name) %>% 
    do(data.frame(bd_time = fn_bd_components(.[,taxon.list],
                                             bd_component_name = 'D')))
  
  # 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
  dat.in.regional.means <- dat.in.wide.spp %>% 
    group_by_(.dots = time_step_name) %>%
    select(one_of(c(taxon.list, time_step_name))) %>%
    summarise_each(funs(mean(., na.rm = TRUE)))
  gamma_temporal_bd <- fn_bd_components(dat.in.regional.means[,taxon.list],
                                        bd_component_name = 'D')
  
  # 3 -- 
  mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time, na.rm = TRUE)
  
  # 4 -- 
  phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd
  
  dat.bd.total <- data.frame(
    beta_type = 'total',
    gamma_temporal_bd,
    mean_alpha_temporal_bd,
    phi_bd,
    n_locations = length(unique(dat.in.wide.spp[,location_name])),
    n_times = length(unique(dat.in.wide.spp[,time_step_name])))
  
  ################
  # -- total BD repl
  ################
  # 1 -- temporal beta-div at each site
  alpha_temporal_bd <- dat.in.wide.spp %>% 
    group_by_(.dots = location_name) %>% 
    do(data.frame(bd_time = fn_bd_components(.[,taxon.list],
                                             bd_component_name = 'repl')))
  
  # 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
  dat.in.regional.means <- dat.in.wide.spp %>% 
    group_by_(.dots = time_step_name) %>%
    select(one_of(c(taxon.list, time_step_name))) %>%
    summarise_each(funs(mean(., na.rm = TRUE)))
  gamma_temporal_bd <- fn_bd_components(dat.in.regional.means[,taxon.list],
                                        bd_component_name = 'repl')
  
  # 3 -- 
  mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time, na.rm = TRUE)
  
  # 4 -- 
  phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd
  
  dat.bd.total.repl <- data.frame(
    beta_type = 'total_repl',
    gamma_temporal_bd,
    mean_alpha_temporal_bd,
    phi_bd,
    n_locations = length(unique(dat.in.wide.spp[,location_name])),
    n_times = length(unique(dat.in.wide.spp[,time_step_name])))
  
  ################
  # -- total BD rich
  ################
  # 1 -- temporal beta-div at each site
  alpha_temporal_bd <- dat.in.wide.spp %>% 
    group_by_(.dots = location_name) %>% 
    do(data.frame(bd_time = fn_bd_components(.[,taxon.list],
                                             bd_component_name = 'rich')))
  
  # 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
  dat.in.regional.means <- dat.in.wide.spp %>% 
    group_by_(.dots = time_step_name) %>%
    select(one_of(c(taxon.list, time_step_name))) %>%
    summarise_each(funs(mean(., na.rm = TRUE)))
  gamma_temporal_bd <- fn_bd_components(dat.in.regional.means[,taxon.list],
                                        bd_component_name = 'rich')
  
  # 3 -- 
  mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time, na.rm = TRUE)
  
  # 4 -- 
  phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd
  
  dat.bd.total.rich <- data.frame(
    beta_type = 'total_rich',
    gamma_temporal_bd,
    mean_alpha_temporal_bd,
    phi_bd,
    n_locations = length(unique(dat.in.wide.spp[,location_name])),
    n_times = length(unique(dat.in.wide.spp[,time_step_name])))
  
  ################
  # -- turnover "rate"
  ################
  
  # 1 -- temporal beta-div at each site
  alpha_temporal_bd <- dat.in.wide.spp %>% 
    group_by_(.dots = location_name) %>% 
    do(data.frame(bd_time = fn_bd_components_mean_turnover_rate(.[,taxon.list],
                                             bd_component_name = 'D')))
  
  # 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
  dat.in.regional.means <- dat.in.wide.spp %>% 
    group_by_(.dots = time_step_name) %>%
    select(one_of(c(taxon.list, time_step_name))) %>%
    summarise_each(funs(mean(., na.rm = TRUE)))
  gamma_temporal_bd <- fn_bd_components_mean_turnover_rate(
    dat.in.regional.means[,taxon.list],
    bd_component_name = 'D')
  
  # 3 -- 
  mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time, na.rm = TRUE)
  
  # 4 -- 
  phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd
  
  dat.bd.turnover_rate <- data.frame(
    beta_type = 'turnover_rate',
    gamma_temporal_bd,
    mean_alpha_temporal_bd,
    phi_bd,
    n_locations = length(unique(dat.in.wide.spp[,location_name])),
    n_times = length(unique(dat.in.wide.spp[,time_step_name])))  
  
  ################
  # -- turnover "rate" repl
  ################
  
  # 1 -- temporal beta-div at each site
  alpha_temporal_bd <- dat.in.wide.spp %>% 
    group_by_(.dots = location_name) %>% 
    do(data.frame(bd_time = fn_bd_components_mean_turnover_rate(.[,taxon.list],
                                                                bd_component_name = 'repl')))
  
  # 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
  dat.in.regional.means <- dat.in.wide.spp %>% 
    group_by_(.dots = time_step_name) %>%
    select(one_of(c(taxon.list, time_step_name))) %>%
    summarise_each(funs(mean(., na.rm = TRUE)))
  gamma_temporal_bd <- fn_bd_components_mean_turnover_rate(
    dat.in.regional.means[,taxon.list],
    bd_component_name = 'repl')
  
  # 3 -- 
  mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time, na.rm = TRUE)
  
  # 4 -- 
  phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd
  
  dat.bd.turnover_rate_repl <- data.frame(
    beta_type = 'turnover_rate_repl',
    gamma_temporal_bd,
    mean_alpha_temporal_bd,
    phi_bd,
    n_locations = length(unique(dat.in.wide.spp[,location_name])),
    n_times = length(unique(dat.in.wide.spp[,time_step_name])))
  
  ################
  # -- turnover "rate" rich
  ################
  
  # 1 -- temporal beta-div at each site
  alpha_temporal_bd <- dat.in.wide.spp %>% 
    group_by_(.dots = location_name) %>% 
    do(data.frame(bd_time = fn_bd_components_mean_turnover_rate(.[,taxon.list],
                                                                bd_component_name = 'rich')))
  
  # 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
  dat.in.regional.means <- dat.in.wide.spp %>% 
    group_by_(.dots = time_step_name) %>%
    select(one_of(c(taxon.list, time_step_name))) %>%
    summarise_each(funs(mean(., na.rm = TRUE)))
  gamma_temporal_bd <- fn_bd_components_mean_turnover_rate(
    dat.in.regional.means[,taxon.list],
    bd_component_name = 'rich')
  
  # 3 -- 
  mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time, na.rm = TRUE)
  
  # 4 -- 
  phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd
  
  dat.bd.turnover_rate_rich <- data.frame(
    beta_type = 'turnover_rate_rich',
    gamma_temporal_bd,
    mean_alpha_temporal_bd,
    phi_bd,
    n_locations = length(unique(dat.in.wide.spp[,location_name])),
    n_times = length(unique(dat.in.wide.spp[,time_step_name])))
  
  # combine results
  dat.bd <- rbind(dat.bd.turnover_rate,
                  dat.bd.turnover_rate_repl,
                  dat.bd.turnover_rate_rich,
                  dat.bd.total,
                  dat.bd.total.repl,
                  dat.bd.total.rich
  )
  
  return(dat.bd)
}

#######################################################
# -- download list of data sets off google drive using google-id
#######################################################
id_google_data_list <- '0B2P104M94skvc0ZCd214N1hKczQ'
download.link <- paste0("https://drive.google.com/uc?export=download&id=",
                        id_google_data_list)

data_list <- read.csv(file = download.link, 
                      header = T,
                      stringsAsFactors = FALSE) %>% 
  select(LTER.site, organism, body.size, trophic.group, biome, google.id) %>%
  filter(LTER.site != 'EXAMPLE') %>%
  filter(nchar(google.id) > 0)
#######################################################
#######################################################
#######################################################
#######################################################

# test cases that work
i <- 9 #NWT
i <- 4 #CAP birds


data_ALL <- data.frame()
for(i in 1:nrow(data_list)){
  try({
    i_data_record <- data_list[i,]
    data_id_googledrive <- i_data_record$google.id
    
    download.link <- paste0("https://drive.google.com/uc?export=download&id=",
                            data_id_googledrive)
    
    d.in.long <- read.csv(file = download.link, header = T,
                          stringsAsFactors = FALSE) %>%
      filter(OBSERVATION_TYPE == 'TAXON_COUNT')
    
    d.bd <- fn_comp_stability_components(d.in.long,
                                         location_name = 'SITE_ID',
                                         time_step_name = 'DATE',
                                         taxon_name = 'VARIABLE_NAME',
                                         taxon_count_name = 'VALUE')
    
    data_ALL <- rbind(
      data_ALL,
      data.frame(i_data_record[1,],
                 d.bd))
  })
  print(i_data_record)
}

# write.csv(data_ALL, 'Group3-diversity-metrics/data-comp-stability-components.csv')
    