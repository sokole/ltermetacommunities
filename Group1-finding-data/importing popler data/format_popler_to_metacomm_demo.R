library(tidyverse)
library(popler)


# select variables for exploring obsevational studies
var_sel   <- c("title", "proj_metadata_key", "lterid", "datatype", "studytype", "duration_years", "n_of_taxas", "metalink")


# observational community data with more than 20 years
obs_raw   <- browse(duration_years>20 & studytype == 'obs' & community == 'yes' & lterid != 'SBC' & tot_spat_rep > 5,
                    trim=F, full_tbl=T)


# get number of taxas
taxa_n    <- lapply(1:nrow(obs_raw), function(ii) obs_raw[ii,]$taxas[[1]] %>% nrow)
obs_stud  <- obs_raw %>%
                mutate(n_of_taxas = unlist(taxa_n) ) %>%
                select(var_sel)




# format SGS biomass data -----------------------------------------------------------------------------


# download SGS biomass data from popler
sgs_biom_raw <- get_data(proj_metadata_key == 65)


# Format TAXON COUNT
sgs_biom_ab  <- sgs_biom_raw %>%
                  # Unpack all covariates in the popler database
                  cbind( cov_unpack(.) ) %>% 
                  mutate( OBSERVATION_TYPE = 'TAXON_COUNT',
                          # Site id is a combination of the three nested spatial levels
                          SITE_ID          = paste(spatial_replication_level_1, 
                                                   spatial_replication_level_2, 
                                                   spatial_replication_level_3, sep = "_"),
                          DATE             = X3_value,
                          # in this case X2_value == 'Species name"
                          VARIABLE_NAME    = X2_value,
                          VARIABLE_UNITS   = NA,
                          VALUE            = abundance_observation) %>% 
                  select( OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE)


    
# Spatial coordinates


# function to convert lat/lon from factor, to character, to numeric
fac_char_num <- function(x) x %>% as.character %>% as.numeric


# Format LATITUDE
sgs_biom_lat <- sgs_biom_raw %>%
                  cbind( cov_unpack(.) ) %>% 
                  mutate( OBSERVATION_TYPE = 'SPATIAL_COORDINATE',
                          SITE_ID          = paste(spatial_replication_level_1, 
                                                   spatial_replication_level_2, 
                                                   spatial_replication_level_3, sep = "_"),
                          DATE             = NA,
                          # in this case X2_value == 'Species name"
                          VARIABLE_NAME    = 'LATITUDE',
                          VARIABLE_UNITS   = 'decimal.degrees',
                          VALUE            = fac_char_num(X4_value) ) %>% 
                  select( OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE) %>% 
                  unique


# Format LONGITUDE
sgs_biom_lon <- sgs_biom_raw %>%
                  cbind( cov_unpack(.) ) %>% 
                  mutate( OBSERVATION_TYPE = 'SPATIAL_COORDINATE',
                          SITE_ID          = paste(spatial_replication_level_1, 
                                                   spatial_replication_level_2, 
                                                   spatial_replication_level_3, sep = "_"),
                          DATE             = NA,
                          # in this case X2_value == 'Species name"
                          VARIABLE_NAME    = 'LONGITUDE',
                          VARIABLE_UNITS   = 'decimal.degrees',
                          VALUE            = fac_char_num(X5_value) ) %>% 
                  select( OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE) %>% 
                  unique


# put it all together in one data frame
SGS_BIOMASS <- Reduce(function(...) rbind(...), list(sgs_biom_lat,sgs_biom_lon,sgs_biom_lat) )


# Store SGS biomass data
write.csv(SGS_BIOMASS, 'SGS_BIOMASS.csv', row.names=F)
Store SGS biomass data
write.csv(SGS_BIOMASS, 'SGS_BIOMASS.csv', row.names=F)
<!--stackedit_data:
eyJoaXN0b3J5IjpbLTEzNjI3ODgxNjRdfQ==
-->