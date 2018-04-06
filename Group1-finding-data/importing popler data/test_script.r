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


# format AND cover data -----------------------------------------------------------------------------

# download AND cover data from popler
AND_biom_raw <- get_data(proj_metadata_key == 145)

# Format TAXON COUNT
AND_biom_ab  <- AND_biom_raw %>%
  # Unpack all covariates in the popler database
  cbind( cov_unpack(.) ) %>% 
  mutate( OBSERVATION_TYPE = 'TAXON_COUNT',
          # Site id is a combination of the three nested spatial levels
          SITE_ID          = paste(spatial_replication_level_1, 
                                   spatial_replication_level_2, 
                                   # spatial_replication_level_3, 
                                   sep = "_"),
          DATE             = year,
          # in this case X2_value == 'Species name"
          VARIABLE_NAME    = datatype,
          VARIABLE_UNITS   = 'proportion',
          VALUE            = abundance_observation) %>% 
  select( OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE)
