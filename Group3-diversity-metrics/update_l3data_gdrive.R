# script to update l3 data list and categorical data

library(tidyverse)
library(googledrive)

working_dir <- drive_ls(path = as_id("0BxUZSA1Gn1HZamlITk9DZzc1c1E"))
data_list <- working_dir %>% filter(grepl('(?i)\\.csv', name)) %>% select(-drive_resource)

old_list <- read_csv(file = paste0("https://docs.google.com/spreadsheets/export?id=",
  "1_IyFiruxf8vp_DY_q_5YaPmoZSUp0l2jnx9v7B1Ujf8",
  "&format=csv"))

new_data_list <- left_join(data_list, old_list)                       

write_csv(new_data_list, path = "Group3-diversity-metrics/l3_data.csv")

# # link to read in spreadsheet from google drive
# l0_id <- "1wP_-hmmB81cpGklhBZRZybDrVOWaDX2rijBVcrlAdfQ"
# 
# download_l0 <-paste0("https://docs.google.com/spreadsheets/export?id=",
#                      l0_id,
#                      "&format=csv")
# 
# l0 <- read_csv(file = download_l0)
# 
# l0 <- l0 %>% select(`data directory`, organism, `body size`, `dispersal type`, `trophic group`, biome) %>% 
#   mutate(name = paste0("L3-", str_to_lower(`data directory`), ".csv")) %>% 
#   select(-'data directory')

# for(i in 1:length(l0$name)){
#   site.i <- strsplit(l0$name, split = '-')[[i]][2]
#   print(site.i)
# }


drive_update(file = as_id("1_IyFiruxf8vp_DY_q_5YaPmoZSUp0l2jnx9v7B1Ujf8"),
             media = "Group3-diversity-metrics/l3_data.csv")
