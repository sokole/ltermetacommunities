library(tidyverse)
library(lubridate)

#devtools::install_github("AldoCompagnoni/popler", build_vignettes = TRUE)
library(popler)

# devtools::install_github('EDIorg/ecocomDP')
library(ecocomDP)

# load format_popler_to_ecocomDP --- 
# input params are 
#           -- path.out
#           -- proj.metadata.key

my_git_ltermetacommunities_file_path <- getwd()
path_to_script <- paste0(my_git_ltermetacommunities_file_path,'/EDI/format_popler_to_ecocomDP.R')
source(path_to_script)

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
  select(var_sel) %>%
  filter(n_of_taxas >= 5)

data_catalog <- obs_stud
# -----------------------------------------
# set path out
write_csv_directory_path <- 'C:\\Users\\esokol\\Google Drive\\LTER Metacommunities\\LTER-DATA\\Popler\\popler_obs_data_ecocomDP'
if(!file.exists(write_csv_directory_path)) dir.create(write_csv_directory_path)

data_catalog$data_transitioned_to_ecocomDP <- NA

# popler_tables_list <- list()
for(i_row_data_catalog in 1:nrow(data_catalog)){
  
  # get metadata lookup key
  i_proj_key <- data_catalog$proj_metadata_key[i_row_data_catalog]
  
  popler_tables_temp <- list()
  # retrieve and transition data set, save to google drive (locally)
  try({
    popler_tables_temp <- format_popler_to_ecocomDP(
      path.out = write_csv_directory_path,
      write.tables.to.csv = TRUE,
      return.tables.as.list = FALSE, #keep RAM usage from exploding
      proj.metadata.key = i_proj_key
    )
  })
  
  # popler_tables_list[[as.character(i_proj_key)]] <- popler_tables_temp
  data_catalog$data_transitioned_to_ecocomDP[i_row_data_catalog] <- ifelse(length(popler_tables_temp) >0 ,'Y','N')
}

write.csv(data_catalog, file = paste0(write_csv_directory_path,
                                      '\\data_catalog_popler_to_ecocomDP_', Sys.Date(),'.csv'), 
          row.names = FALSE)
