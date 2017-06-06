# ----------------------------------------------- #
#  Explore environmental data - 19 September 2016 #
# ----------------------------------------------- #

# Clear environment
rm(list = ls())

# Set your working environment to the GitHub repository, e.g.: 
#setwd("~/Documents/ltermetacommunities")

#Check to make sure working directory is correct
if(basename(getwd())!="ltermetacommunities"){cat("Plz change your working directory. It should be 'ltermetacommunities'")}

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
#Source data set of interest. The choices are:

# SBC LTER (Santa Barbara Coastal): Macroalgae
data.set <- "SBC-algae"

# SBC LTER (Santa Barbara Coastal): Sessile invertebrates
data.set <- "SBC-sessile_invert"

# SBC LTER (Santa Barbara Coastal): Mobile invertebrates
data.set <- "SBC-mobile_invert"

# SBC LTER (Santa Barbara Coastal): Fishes
data.set <- "SBC-fish"

# NWT LTER (Niwot Ridge): Plants
data.set <- "NWT"

# JRN LTER (Jornada): Lizards
data.set <- "JRN-lizard"

source(paste("Group2-explore-data/format_data/", data.set, "_format_data.R", sep=""))
summary(dat)

# ---------------------------------------------------------------------------------------------------

#Plot time series of environmental variabLes for ech site
env.plot <- ggplot(data = dat$env.long, aes(x = DATE, y = VALUE)) +
  geom_line(aes(color = SITE_ID)) +
  facet_wrap(~ VARIABLE_NAME, scales = "free", ncol = 1) +
  theme_bw()
env.plot

#Look at pairwise correlation between environmental predictors (NOTE: Data must be in wide form)

str(dat$env.wide)
pairs(dat$env.wide[,-(1:4)])
