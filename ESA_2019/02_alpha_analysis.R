#######################################################
# Calculates compositional turnover at local (CV_alpha) and regional (CV_gamma) scales
# Calculates phi, which is scaling of variability, as CV_gamma / CV_alpha
# version 1.0.2 from ltermetacommunities group 3 directory on github
# Author: Eric Sokol (sokole@gmail.com)
#     modified by Nathan Wisnoski 
#     modified by Eric Sokol (2/4/2019)
# based on work in collaboration with Thomas Lamy

# clear out workspace
rm(list=ls())
gc()

# set up options
options(stringsAsFactors = FALSE)

#########################
# libraries
#########################
library(tidyverse)
library(googledrive)

# install ltmc package from github
installed_package_list <- installed.packages() %>% as.data.frame()
if(!'ltmc' %in% installed_package_list$Package){
  devtools::install_github('sokole/ltermetacommunities/ltmc')
}

library(ltmc)

#######################################################
# -- download list of data sets off google drive using google-id
#######################################################

# working_dir <- drive_ls(path = as_id("0BxUZSA1Gn1HZamlITk9DZzc1c1E"))
working_dir <- googledrive::drive_ls('LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space') #human readable path to directory
data_list <- working_dir %>% filter(grepl('(?i)\\.csv', name))


#######################################################
# -- Loop through data sets and call functions
#######################################################


# loop to read in data, call wrapper function, write results to data_ALL
# data_ALL <- data.frame()
analysis_results <- data.frame()

# i = 6 -- should work
# i = 14 -- L3-and-plants-mtStHelens.csv is problem record


for(i in 1:nrow(data_list)){
  messages_i <- character(0)
  
  # initialize results table
  analysis_results_i <- data.frame()
  
  try_result <- try({
    
    # get record
    i_data_record <- data_list[i,]
    
    # get google id used to download data
    data_id_googledrive <- i_data_record$id
    
    # link to read in spreadsheet from google drive
    download.link <- paste0("https://drive.google.com/uc?export=download&id=",
                            data_id_googledrive)
    
    # blank data frame
    d.in.long <- data.frame()
    d.in.long.raw <- data.frame()
    
    # read data into data.frame and filter out non-taxon data, spp names that are NAs, and negative VALUES
    # make SITE_ID and DATE chars
    try({
      d.in.long.raw <- read.csv(file = download.link, header = T,
                                stringsAsFactors = FALSE) 
      
      names(d.in.long.raw) <- toupper(names(d.in.long.raw))
      
      expected_col_names <- c('OBSERVATION_TYPE',
                              'SITE_ID',
                              'DATE',
                              'VARIABLE_NAME',
                              'VARIABLE_UNITS',
                              'VALUE')
      
      missing_col_names <- expected_col_names %>% dplyr::setdiff(names(d.in.long.raw))
      if(length(missing_col_names) > 0){
        msg_tmp <- paste0('WARNING: missing column names: ', 
                          paste(missing_col_names, collapse = ', '), 
                          ' for ', i_data_record$name)
        message(msg_tmp)
        messages_i <- paste(c(messages_i, msg_tmp), collapse = ' | ')
        # next
      }else{
        d.in.long <- d.in.long.raw %>%
          filter(OBSERVATION_TYPE == 'TAXON_COUNT') %>% 
          filter(!is.na(VARIABLE_NAME)) %>%
          filter(VALUE >= 0) %>%
          mutate(SITE_ID = as.character(SITE_ID),
                 DATE = as.character(DATE)) %>%
          select(DATE, SITE_ID, OBSERVATION_TYPE, VARIABLE_NAME, VALUE, VARIABLE_UNITS)
      }
    })
    
    # some error messaging
    if(nrow(d.in.long.raw) == 0){
      msg_tmp <- paste0('WARNING: no data for ',i_data_record$name)
      message(msg_tmp)
      messages_i <- paste(c(messages_i, msg_tmp), collapse = ' | ')
      
    }else if(nrow(d.in.long) == 0){
      msg_tmp <- paste0('WARNING: data not formatted correctly for ',i_data_record$name)
      message(msg_tmp)
      messages_i <- paste(c(messages_i, msg_tmp), collapse = ' | ')
      
    }else if(length(d.in.long$VARIABLE_UNITS %>% unique()) > 1){
      msg_tmp <- paste0('WARNING: more than one unit used to report TAXON_COUNTs for ',i_data_record$name)
      message(msg_tmp)
      messages_i <- paste(c(messages_i, msg_tmp), collapse = ' | ')
      
    }else{
      # Only use control plots if there is a "treatment" in the data set
      if('TREATMENT' %in% names(d.in.long)){
        d.in.long <- d.in.long %>%
          filter(TREATMENT %in% c(NA, 'NA', '', 'control', 'Control','CONTROL'))
        
        # messaging
        msg_tmp <- paste0('WARNING: treatment plots removed from data for ',i_data_record$name)
        message(msg_tmp)
        messages_i <- paste(c(messages_i, msg_tmp), collapse = ' | ')
      }
      
      key_list <- with(d.in.long, 
                       paste(
                         OBSERVATION_TYPE,
                         SITE_ID,
                         DATE,
                         VARIABLE_NAME,
                         VALUE,
                         sep = '_'))
      
      # get rid of exact dupes
      rows2check <- which(duplicated(key_list))
      if(length(rows2check) > 0){
        d.in.long <- data.frame(d.in.long[-rows2check,], row.names = NULL)
        
        # messaging
        msg_tmp <- paste0('WARNING: exact duplicates removed from data for ',i_data_record$name)
        message(msg_tmp)
        messages_i <- paste(c(messages_i, msg_tmp), collapse = ' | ')
      }
      
      # take mean of replicate observations that are not exact dupes
      d.in.long <- d.in.long %>% group_by(
        OBSERVATION_TYPE, SITE_ID, DATE, 
        VARIABLE_NAME, VARIABLE_UNITS) %>%
        summarise(VALUE = mean(VALUE)) %>% data.frame()
    }
    
    
    
    # Send data to function to calclulate compositional variability and scaling of variability
    d.bd.h <- data.frame()
    d.bd.hT <- data.frame()
    d.bd.agg <- data.frame()
    div.part.var <- data.frame()
    
    if(nrow(d.in.long) > 0){
      
      #########################
      # -- metacomm_variability metrics
      
      start_year <- NA
      end_year <- NA
      n_years_observed <- NA
      study_duration <- NA
      
      start_year <- min(as.numeric(d.in.long$DATE))
      end_year <- max(as.numeric(d.in.long$DATE))
      n_years_observed <- d.in.long$DATE %>% unique() %>% length()
      study_duration <- end_year - start_year
      
      
      alpha_div_stab <- data.frame()
      for(j in unique(d.in.long$SITE_ID)){
        current_site <- filter(d.in.long, SITE_ID == j)
        
        # d.bd.h
        d.bd.h <- data.frame()
        d.bd.h <- ltmc::metacommunity_variability(
        data_long = current_site,
        site_id_col_name = 'SITE_ID',
        time_step_col_name = 'DATE',
        taxon_id_col_name = 'VARIABLE_NAME',
        biomass_col_name = 'VALUE',
        standardization_method = 'h',
        variability_type = 'com') %>% 
        as.data.frame() %>%
        mutate(SITE_ID = j, 
               comp_gamma_var_rate = gamma_var / study_duration,
               comp_alpha_var_rate = alpha_var / study_duration)  %>% 
       select(-standardization_method, -variability_type, -alpha_var, -gamma_var)
      
        # # d.bd.hT
        # d.bd.hT <- data.frame()
        # d.bd.hT <- ltmc::metacommunity_variability(
        #   data_long = current_site,
        #   site_id_col_name = 'SITE_ID',
        #   time_step_col_name = 'DATE',
        #   taxon_id_col_name = 'VARIABLE_NAME',
        #   biomass_col_name = 'VALUE',
        #   standardization_method = 'hT',
        #   variability_type = 'com') %>% 
        #   as.data.frame() %>%
        #   mutate(SITE_ID = j, 
        #          gamma_var_rate = gamma_var / study_duration,
        #          alpha_var_rate = alpha_var / study_duration) 
        # 
        # d.bd.agg
        d.bd.agg <- data.frame()
        d.bd.agg <- ltmc::metacommunity_variability(
          data_long = current_site,
          site_id_col_name = 'SITE_ID',
          time_step_col_name = 'DATE',
          taxon_id_col_name = 'VARIABLE_NAME',
          biomass_col_name = 'VALUE',
          variability_type = 'agg')  %>% 
          as.data.frame() %>%
          mutate(SITE_ID = j,
                 agg_gamma_var_rate = gamma_var / study_duration,
                 agg_alpha_var_rate = alpha_var / study_duration) %>% 
          select(-standardization_method, -variability_type, -alpha_var, -gamma_var)
        
        div.part <- data.frame(
        ltmc::divpart(
          data_long = current_site, 
          site_id_col_name = 'SITE_ID',
          time_step_col_name = 'DATE',
          taxon_id_col_name = 'VARIABLE_NAME',
          biomass_col_name = 'VALUE')) %>% 
        select(alpha_div) %>% 
        summarize(mean_alpha = mean(alpha_div)) %>% 
        mutate(SITE_ID = j)
        
        # now join
        site_output <- left_join(div.part, d.bd.h) %>% left_join(d.bd.agg)
      alpha_div_stab <- rbind.data.frame(alpha_div_stab, site_output)
      }  
      
      
      # combine results in one long-fromat dataframe
      analysis_results_i <- data.frame(
        dataset_file_name = i_data_record$name,
        dataset_google_id = i_data_record$id,
        start_year = start_year,
        end_year= end_year,
        n_years_observed = n_years_observed,
        study_duration = study_duration,
        organism_count_type = d.in.long$VARIABLE_UNITS %>% unique() %>% paste(collapse = ' | '),
        bind_rows(
          alpha_div_stab))
    }
    
  })
  
  # add a messages column if necessary
  if(length(messages_i) > 0 & nrow(analysis_results_i) == 0){
    analysis_results_i <- data.frame(
      dataset_file_name = i_data_record$name,
      dataset_google_id = i_data_record$id,
      messages = messages_i)
  }else if(length(messages_i) > 0 & nrow(analysis_results_i) > 0){
    analysis_results_i[,'messages'] <- messages_i
  }
  
  #combine results
  analysis_results <- bind_rows(
    analysis_results,
    analysis_results_i)
  
  print(i_data_record$name)
}

# #write results locally
# write.csv(analysis_results, 
#           file.path("Group3-diversity-metrics", 
#                     paste0('analysis_results_', Sys.Date(), '.csv')),
#           row.names = FALSE)

#####################################
# code to write as results as a .csv
write_path <- 'ESA_2019/data/'

# check write path

write_filename <- paste0(write_path,'L4_alpha_variability_analysis_results_', Sys.Date(), '.csv')

# temp write local
readr::write_csv(analysis_results, write_filename)



# #######################################################
# # -- Make figures
# #######################################################
# env_data <- read_csv(file = "Group3-diversity-metrics/l3_data.csv")
# 
# data_ALL <- data_ALL %>% mutate(l3_filename = name)
# # how do alpha, gamma, and beta variability over a metacom richness gradient?
# 
# left_join(data_ALL, env_data)  %>% filter(gamma_mean < 100) %>% 
#   gather(gamma_temporal_bd, mean_alpha_temporal_bd, phi_bd, key = "scale", value = "bd") %>% 
#   ggplot(aes(x = gamma_mean, y = bd, color = biome, fill = biome)) + 
#   facet_grid(scale ~ ., scales = "free_y") +
#   geom_point(alpha = 0.5) + 
#   geom_smooth(method = 'lm', formula = y ~ x, alpha = 0.15) + 
#   theme_minimal() +
#   ggsave("Group3-diversity-metrics/figures/gammadiv-stability.png",
#          width = 7, height = 7, dpi = 300, units = "in")
# 
# left_join(data_ALL, biomes) %>% filter(alpha_mean < 100) %>% 
#   gather(gamma_temporal_bd, mean_alpha_temporal_bd, phi_bd, key = "scale", value = "bd") %>% 
#   ggplot(aes(x = alpha_mean, y = bd, color = biome, fill = biome)) + 
#   facet_grid(scale ~ ., scales = "free_y") +
#   geom_point(alpha = 0.5) + 
#   geom_smooth(method = 'lm', formula = y ~ x, alpha = 0.15) + 
#   theme_minimal() + 
#   ggsave("Group3-diversity-metrics/figures/alphadiv-stability.png",
#          width = 7, height = 7, dpi = 300, units = "in")
# 
# left_join(data_ALL, env_data)  %>% 
#   gather(gamma_temporal_bd, mean_alpha_temporal_bd, phi_bd, key = "scale", value = "bd") %>% 
#   ggplot(aes(x = `mobility`, y = bd, color = `dispersal habit`, fill = `dispersal habit`)) + 
#   facet_grid(scale ~ ., scales = "free_y") +
#   geom_boxplot(alpha = 0.5) + 
#   geom_point(alpha = 0.25) +
#   theme_minimal() +
#   ggsave("Group3-diversity-metrics/figures/dispersaltype-stability.png",
#          width = 7, height = 7, dpi = 300, units = "in")
# 
# left_join(data_ALL, env_data)  %>% 
#   gather(gamma_temporal_bd, mean_alpha_temporal_bd, phi_bd, key = "scale", value = "bd") %>% 
#   ggplot(aes(x = `trophic group`, y = bd, color = `trophic group`, fill = `trophic group`)) + 
#   facet_grid(scale ~ ., scales = "free_y") +
#   geom_boxplot(alpha = 0.5) + 
#   geom_point(alpha = 0.25) +
#   theme_minimal() +
#   ggsave("Group3-diversity-metrics/figures/trophicgroup-stability.png",
#          width = 7, height = 7, dpi = 300, units = "in")
# 
# left_join(data_ALL, env_data)  %>% filter(gamma_mean < 100) %>% 
#   gather(gamma_temporal_bd, mean_alpha_temporal_bd, phi_bd, key = "scale", value = "bd") %>% 
#   ggplot(aes(x = `body size`, y = bd, color = `body size`, fill = `body size`)) + 
#   facet_grid(scale ~ ., scales = "free_y") +
#   geom_boxplot(alpha = 0.5) + 
#   geom_point(alpha = 0.25) +
#   theme_minimal() +
#   ggsave("Group3-diversity-metrics/figures/bodytype-stability.png",
#          width = 7, height = 7, dpi = 300, units = "in")
# 
# left_join(data_ALL, env_data)  %>% filter(gamma_mean < 100) %>% 
#   gather(gamma_temporal_bd, mean_alpha_temporal_bd, phi_bd, key = "scale", value = "bd") %>% 
#   ggplot(aes(x = `organism`, y = bd)) + 
#   facet_grid(scale ~ ., scales = "free_y") +
#   geom_boxplot(alpha = 0.5) + 
#   geom_point(alpha = 0.25) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   ggsave("Group3-diversity-metrics/figures/organism-stability.png",
#          width = 7, height = 7, dpi = 300, units = "in")
