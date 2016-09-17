# Updating previous functions to work with SBC dataset

# Read data from Google Drive using tips from this page: 
# http://www.labnol.org/internet/direct-links-for-google-drive/28356/
# Get id number from sharing link and use to create download link

library(tidyr)
library(dplyr)

download.link <- "https://drive.google.com/uc?export=download&id=0BxUZSA1Gn1HZYTVfd2FZTWhWbm8"
sbc.data <- read.csv(file = download.link, header = T)

# Create the spatial dataset
sbc.spatial.long <- sbc.data[which(sbc.data$OBSERVATION_TYPE == "SPATIAL_COORDINATE"),
                        which(colnames(sbc.data) %in% c("SITE_ID", "VARIABLE_NAME", "VALUE"))]
sbc.spatial.wide <- tidyr::spread(sbc.spatial.long, key = VARIABLE_NAME, value = VALUE)

# Create the environmental dataset
sbc.env.long <- sbc.data[which(sbc.data$OBSERVATION_TYPE == "ENV_VAR"),
                             which(colnames(sbc.data) %in% 
                                     c("SITE_ID", "DATE",
                                       "VARIABLE_NAME", "VALUE"))]

sbc.env.wide <- tidyr::spread(sbc.env.long, key = VARIABLE_NAME, value = VALUE)

# Create the species matrix
sbc.species.long <- sbc.data[which(sbc.data$OBSERVATION_TYPE == "TAXON_COUNT"),
                         which(colnames(sbc.data) %in% 
                                 c("SITE_ID", "DATE",
                                   "VARIABLE_NAME", "VALUE"))]
sbc.species.wide <- tidyr::spread(sbc.species.long, key = VARIABLE_NAME, value = VALUE, fill = 0)
