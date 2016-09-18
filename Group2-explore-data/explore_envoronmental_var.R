# ------------------------------------- #
# Reformat SBC data - 17 September 2016 #
# ------------------------------------- #

# Set working environment
rm(list = ls())
setwd("~/Google Drive/LTER-DATA/SBC-Lamy-Castorani/")

# Check for and install required packages
library()

for (package in c('dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom', 'ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


id.long.dat <- "0BxUZSA1Gn1HZYTVfd2FZTWhWbm8"
sbc.long <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.long.dat)) 


dat.long <- sbc.long

# Check types of taxa
unique(dat.long$TAXON_GROUP)

str(dat.long)

#make dataframe of site-year data
#unique(dat.long$OBSERVATION_TYPE)
site_yr <- subset(dat.long, OBSERVATION_TYPE == "ENV_VAR")

str(site_yr)
site_yr <- droplevels(site_yr)


levels(site_yr$VARIABLE_UNITS)
levels(site_yr$VARIABLE_NAME)


#Plot time series of environmental varialbes for ech site

env.plot <- ggplot(data = site_yr, aes(x = DATE, y = VALUE)) +
geom_line(aes(color = SITE_ID)) +
facet_wrap(~ VARIABLE_NAME, scales = "free", ncol = 1) +
theme_bw()
env.plot
? facet_wrap
plot(y = site_yr[site_yr$VARIABLE_NAME == "WAVE_HT_MEAN", ]$VALUE, x = site_yr[site_yr$VARIABLE_NAME == "WAVE_HT_WINTER_MEAN", ]$VALUE)

#Look at pairwise correlation between envoronmental predictors
##NOTE Data must be win wide form
id.env <- "0BxUZSA1Gn1HZZDF5cGpnLWtnWGc" # Google Drive file ID
site_yr_wide <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.env))
str(site_yr_wide)
pairs(site_yr_wide[,c("TEMP_MEAN_C", "WAVE_HT_MEAN","WAVE_HT_WINTER_MEAN")])
