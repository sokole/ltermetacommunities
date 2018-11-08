#update the L3-DATA-List gsheet on google drive
library(tidyverse)
require(googlesheets)

# read Nina's metadata table
auto_metadata <- read_csv('https://raw.githubusercontent.com/sokole/ltermetacommunities/master/MS3-Supp-Info/metadata_table.csv')

# look at L0 dir
L0_list_of_files <- googledrive::drive_ls('LTER Metacommunities/LTER-DATA/L0-raw')
l0_data_list_google_id <- L0_list_of_files %>% filter(name == 'L0-DATA-list') %>% select(id) %>% unlist()

# read in L0 data table
l0_data_list <- l0_data_list_google_id %>%
  gs_key(lookup = TRUE) %>%
  gs_read()

# look in L3 dir
L3_list_of_files <- googledrive::drive_ls('LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space')
l3_data_list_google_id <- L3_list_of_files %>% filter(name == 'L3-DATA-list') %>% select(id) %>% unlist()

# l3_data_list <- l3_data_list_google_id %>%
#   gs_key(lookup = TRUE) %>%
#   gs_read()

# make a list of L3 files
l3_data_csv_list <- L3_list_of_files %>% filter(grepl('(?i)\\.csv', name))

# pull out vars from L0 to propagate to L3 data list
l0_data_list <- l0_data_list %>% select(dataset_id, `data directory`, `LTER site`, 
                                        contact, `responsible person`, `L3 status`, `provided identifier`, `doi`, `date accessed`, `date published`,
                                        source_url_or_contact, `body size`, `dispersal habit`, mobility, `trophic group`, `biome`) %>%
  rename(L0_data_directory = `data directory`)

# format data set names from L3 file list
l3_data_list_filenames_table <- data.frame(
  dataset_id = l3_data_csv_list$name %>% gsub('(?i)L3\\-','',.) %>% gsub('\\.csv','', .),
  l3_filename = l3_data_csv_list$name,
  google_id = l3_data_csv_list$id)

# merge L3 file names, L0 data, and Nina's metadata
l3_data_list_updated <- l3_data_list_filenames_table%>% left_join(l0_data_list, by = 'dataset_id') %>%
  left_join(auto_metadata, by = c('dataset_id' = 'dataset'))

# update the googlesheet L3-DATA-List
googlesheets::gs_edit_cells(
  ss = gs_key(l3_data_list_google_id),
  ws = 'DATA-list',
  input = l3_data_list_updated)

# write to github dir
write_csv(l3_data_list_updated, 'Group3-diversity-metrics/l3_data.csv')

# # Write CSV file for cleaned data (L3)
# # write.csv(out_long, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-hbr-birds-sillett.csv", row.names = F)
# write_path <- '~/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/'
# # check write path
# # googledrive::drive_ls(write_path)

# #####################################
# # code to write as L3-DATA-List as a .csv
# write_filename <- paste0('L3-DATA-List.csv')
# 
# # temp write local
# readr::write_csv(l3_data_list_updated, write_filename)
# googledrive::drive_upload(write_filename,
#              path = write_path,
#              name = write_filename,
#              type = NULL,
#              verbose = TRUE)
# 
# #remove local file
# file.remove(write_filename)

