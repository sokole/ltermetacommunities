# Aldo Compagnoni Nov 2018
# The very start of formatting the HAYS plant dataset
rm(list = ls(all = T))
options(stringsAsFactors = F)
library(dplyr)
library(tidyr)
library(stringi)
library(RCurl)

# read data

# all records 
all_df  <- read.csv( 'C:/Users/ac22qawo/Downloads/allrecords.csv' )

# quadrat info
quad_df <- read.csv( 'C:/Users/ac22qawo/Downloads/quadrat_info.csv' )

# species
spp_df  <- read.csv( 'C:/Users/ac22qawo/Downloads/species_list.csv' )


# 
# # all records
# cia  = url('https://drive.google.com/open?id=1oFaOkGA2P7-oMDTA1v5m343h6dkRteSC')
# cia  = Rcurl::getURL('https://drive.google.com/open?id=1oFaOkGA2P7-oMDTA1v5m343h6dkRteSC')
# all_df    <- read.csv(url('https://drive.google.com/open?id=1oFaOkGA2P7-oMDTA1v5m343h6dkRteSC'),
#                       quote = "",
#                       table = )
# # quad info
# read.csv('C:/Users/ac22qawo/Downloads/quadrat_info.csv')
# 
# cia  <- url('https://drive.google.com/open?id=1J8NdGtpznfkTGxilouFlDNEmPMHjlj6n')
# cia  <- RCurl::getURL('https://drive.google.com/open?id=1J8NdGtpznfkTGxilouFlDNEmPMHjlj6n')
# 
# read.csv( cia ) %>% head
# quad_d <- read.table( 'https://drive.google.com/open?id=1J8NdGtpznfkTGxilouFlDNEmPMHjlj6n',
#                       header=T,
#                       quote = "",
#                       row.names = NULL, 
#                       stringsAsFactors = F )
# 
# cit <- read.csv("citations.CSV", quote = "", 
#                  row.names = NULL, 
#                  stringsAsFactors = FALSE)
# 
# # quad inv
# quad_inv  <- read.csv('https://drive.google.com/open?id=1e3msxBT6rhf2SOAzmOTI9bk87M1UI158')

