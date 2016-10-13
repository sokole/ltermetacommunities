# ----------------------------------------------- #
#  Explore environmental data - 19 September 2016 #
# ----------------------------------------------- #

# Clear environment
rm(list = ls())

# Assign data set of interest

# CAP LTER (Central Arizona-Phoenix)
#data.set <- "CAP-birds-CORE"
#data.key <- "" # Google Drive file ID (different for each dataset)

# NWT LTER (Niwot Ridge)
#data.set <- "NWT-plants-Hallett-and-Sokol"
#data.key <- "" # Google Drive file ID (different for each dataset)

# SBC LTER (Santa Barbara Coastal)
data.set <- "SBC-Lamy-Castorani"
data.key <- "0B7o8j0RLpcxiTnB5a01YU2pWdk0" # Google Drive file ID (different for each dataset)

# ---------------------------------------------------------------------------------------------------
# Set working environment
setwd("~/Google Drive/LTER Metacommunities")

# Check for and install required packages
#library()

for (package in c('dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom', 'ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# ---------------------------------------------------------------------------------------------------
# IMPORT DATA
#read in .Rdata list
#Why doesn't this work?  Help!
#load(sprintf("https://drive.google.com/open?id=", data.key)) 
load(paste("Intermediate_data/",data.set,".Rdata", sep="")) #workaround
summary(dat)
str(dat$env)
# ---------------------------------------------------------------------------------------------------

#Plot time series of environmental variabLes for ech site

env.plot <- ggplot(data = dat$env.long, aes(x = DATE, y = VALUE)) +
geom_line(aes(color = SITE_ID)) +
facet_wrap(~ VARIABLE_NAME, scales = "free", ncol = 1) +
theme_bw()
env.plot

plot(y = env.long[env.long$VARIABLE_NAME == "WAVE_HT_MEAN", ]$VALUE, x = env.long[env.long$VARIABLE_NAME == "WAVE_HT_WINTER_MEAN", ]$VALUE)

#Look at pairwise correlation between envoronmental predictors
##NOTE Data must be in wide form
  env.wide <- env.long %>%
  select(-VARIABLE_UNITS) %>%
    tidyr::spread(VARIABLE_NAME,  VALUE)


str(env.wide)
pairs(dat$env[,c("TEMP_MEAN_C", "WAVE_HT_MEAN","WAVE_HT_WINTER_MEAN")])
