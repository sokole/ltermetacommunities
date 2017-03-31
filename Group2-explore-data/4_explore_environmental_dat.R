# ----------------------------------------------- #
#  Explore environmental data - 19 September 2016 #
# ----------------------------------------------- #

# Clear environment
rm(list = ls())

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

###########################################
# Assign data set of interest
# NOTE: After running the '1_format_raw_data.R' script, each dataset has been stored as an .Rdata list in the "Intermediate_data" folder on Google Drive. The Google Drive file ID is different for each dataset.

# SBC LTER (Santa Barbara Coastal): Macroalgae
data.set <- "SBC-algae-Castorani_Lamy"

# SBC LTER (Santa Barbara Coastal): Sessile invertebrates
data.set <- "SBC-sessile_invert-Castorani_Lamy"

# SBC LTER (Santa Barbara Coastal): Mobile invertebrates
data.set <- "SBC-mobile_invert-Castorani_Lamy"

# SBC LTER (Santa Barbara Coastal): Fishes
data.set <- "SBC-fish-Castorani_Lamy"

# ---------------------------------------------------------------------------------------------------
# IMPORT DATA
load(paste("Intermediate_data/", data.set,".Rdata", sep=""))  # Read in .Rdata list
summary(dat)

# ---------------------------------------------------------------------------------------------------
env.long <- dat$env.long

#Plot time series of environmental variabLes for ech site
env.plot <- ggplot(data = env.long, aes(x = DATE, y = VALUE)) +
  geom_line(aes(color = SITE_ID)) +
  facet_wrap(~ VARIABLE_NAME, scales = "free", ncol = 1) +
  theme_bw()
env.plot

#Look at pairwise correlation between envoronmental predictors (NOTE: Data must be in wide form)
env.wide <- dat$env

str(env.wide)
pairs(dat$env[,c("TEMP_MEAN_C", "WAVE_HT_MEAN","WAVE_HT_WINTER_MEAN")])
