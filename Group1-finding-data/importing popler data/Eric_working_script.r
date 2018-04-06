library(tidyverse)
library(popler)

devtools::install_github('EDIorg/ecocomDP')
library(ecocomDP)

# load format_popler_to_ecocomDP --- 
# input params are 
#           -- path.out
#           -- proj.metadata.key
source('~/Git/ltermetacommunities/EDI/format_popler_to_ecocomDP.R')

# -- read in saved data catalog from gsheet
# read in data popler_observationa_data_catalog_20180216 from gsheets
# gsheet_url <- 'https://docs.google.com/spreadsheets/d/1mBf220huatR85r_RLIJN3ut4_vOBt9yTg9qTzve3nXU/edit#gid=2024054854'
# data_catalog <- gsheet::gsheet2tbl(gsheet_url)

# # get proj_metadata_key for records in data_catalog
# data_catalog$proj_metadata_key <- NA
# for(i_row in 1:nrow(data_catalog)){
#   try({
#     data_catalog$proj_metadata_key[i_row] <- (popler::browse(title == data_catalog$title[i_row]))$proj_metadata_key
#   })
# }


# -- query popler to produce a data catalog
# select variables for exploring obsevational studies
var_sel   <- c("title", "proj_metadata_key", "lterid", "datatype", "studytype", "duration_years", "n_of_taxas", "metalink")


# observational community data with more than 10 years
obs_raw   <- browse(duration_years>5 & studytype == 'obs' & community == 'yes' & lterid != 'SBC' & tot_spat_rep > 5,
                    trim=F, full_tbl=T)

# get number of taxas
taxa_n    <- lapply(1:nrow(obs_raw), function(ii) obs_raw[ii,]$taxas[[1]] %>% nrow)
obs_stud  <- obs_raw %>%
  mutate(n_of_taxas = unlist(taxa_n) ) %>%
  select(var_sel)

data_catalog <- obs_stud
# -----------------------------------------
# set path out
if(!file.exists('popler_obs_data_ecocomDP')) dir.create('popler_obs_data_ecocomDP')


i_proj_key <- data_catalog$proj_metadata_key[4]
format_popler_to_ecocomDP(
  proj.metadata.key = i_proj_key,
  path.out = '~/popler_obs_data_ecocomDP')
