options(stringsAsFactors = FALSE)

library(tidyverse)

# read in data
d_divpart_CVs <- read.csv('dat_CVs_divpart.csv') %>% select(-TAXON_GROUP)
d_comp_turnover <- read.csv('dat-comp-variability-components_2018-05-09.csv') %>% select(-beta_type)

d_temperatures <- read.csv('CV_env.csv') %>%
  dplyr::rename(temp_CV = CV_mean,
         temp_SD = SD_mean)

# combine results into a single table
d_results <- dplyr::left_join(d_comp_turnover, d_divpart_CVs, by = c('google.id','LTER.site'))

d_results <- dplyr::right_join(d_temperatures, d_results, by = c('site' = 'LTER.site')) %>%
  select(data_set, site, google.id, organism, body.size, dispersal.type, trophic.group, biome,
         temp_type, temp_CV, temp_SD,
         start_year, end_year, n_years, n_locations,
         CV_alpha_0, CV_beta_0, CV_gamma_0, 
         CV_alpha_2, CV_beta_2, CV_gamma_2, 
         alpha_temporal_bd_rate, gamma_temporal_bd_rate, 
         phi_bd)

#write locally 
write.csv(d_results,
          paste0('dat-RESULTS',Sys.Date(),'.csv'),
          row.names = FALSE)
