# ----------------------------------------------- #
#  Explore environmental data - 19 September 2016 #
# ----------------------------------------------- #

# Set working environment
rm(list = ls())

# Assign data set of interest

# CAP LTER (Central Arizona-Phoenix)
#data.set <- "CAP-birds-CORE"
#data.key <- "0BzcCZxciOlWgeHJ5SWx1YmplMkE" # Google Drive file ID (different for each dataset)

# NWT LTER (Niwot Ridge)
#data.set <- "NWT-plants-Hallett-and-Sokol"
#data.key <- "0B2P104M94skvQVprSnBsYjRzVms" # Google Drive file ID (different for each dataset)

# SBC LTER (Santa Barbara Coastal)
data.set <- "SBC-Lamy-Castorani"
data.key <- "0BxUZSA1Gn1HZYTVfd2FZTWhWbm8" # Google Drive file ID (different for each dataset)

# 

# Set working environment
setwd(paste("~/Google Drive/LTER-DATA/", data.set, sep=""))

library()

for (package in c('dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom', 'ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# IMPORT DATA
dat.long <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", data.key)) 

str(dat.long)

#make dataframe of site-year data
#unique(dat.long$OBSERVATION_TYPE)
env.long <- subset(dat.long, OBSERVATION_TYPE == "ENV_VAR")
env.long <- droplevels(env.long)
str(env.long)

levels(env.long$VARIABLE_UNITS)
levels(env.long$VARIABLE_NAME)


#Plot time series of environmental variabLes for ech site

env.plot <- ggplot(data = env.long, aes(x = DATE, y = VALUE)) +
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
pairs(env.wide[,c("TEMP_MEAN_C", "WAVE_HT_MEAN","WAVE_HT_WINTER_MEAN")])
