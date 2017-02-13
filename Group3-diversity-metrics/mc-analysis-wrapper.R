# Adapting previous script to work with long form datasets

# Initialize the environment
rm(list=ls())
for (package in c('dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
options(stringsAsFactors = FALSE)

# For now, work in this folder
setwd("./Group1-diversity-metrics/")

# Bring in the data
download.link <- "https://drive.google.com/uc?export=download&id=0BxUZSA1Gn1HZYTVfd2FZTWhWbm8"
download.file(url = download.link, destfile = "./sbc_data.csv")
sbc.data <- read.csv("./sbc_data.csv")

# Source other functions needed to run the analysis
source("long-form-functions.R")
source("../utilities/mc-split.R")

# Function to analyze time series 
fn.mc.analyze <- function(mc.data = mc.data, output = output){

  fn.mc.split(mc.data)
  
  # Create the species matrix
  mc.species.long <- filter(na.omit(mc.data), OBSERVATION_TYPE == "TAXON_COUNT") %>%
    select(SITE_ID, DATE, VARIABLE_NAME, VALUE)
  # mc.species.wide <- tidyr::spread(mc.species.long, key = VARIABLE_NAME, value = VALUE, fill = 0)
  # mc.species.wide <- arrange(
  #   mc.species.wide[,which(colnames(mc.species.wide) != "")], SITE_ID) # removing potential blank cols
  
  mc.analysis.by.timestep <- mc.species.long %>% group_by(DATE) %>% 
    summarise(
      
      
      # DIVERSITY PARTITIONING 
      
      alpha_q0 = fn.divpart.long(
        site.id.vect = SITE_ID,
        spp = VARIABLE_NAME,
        abund = VALUE,
        lev = "alpha",
        q = 0),
      
      beta_q0 = fn.divpart.long(
        site.id.vect = SITE_ID,
        spp = VARIABLE_NAME,
        abund = VALUE,
        lev = "beta",
        q = 0),
      
      gamma_q0 = fn.divpart.long(
        site.id.vect = SITE_ID,
        spp = VARIABLE_NAME,
        abund = VALUE,
        lev = "gamma",
        q = 0),
      
      alpha_q2 = fn.divpart.long(
        site.id.vect = SITE_ID,
        spp = VARIABLE_NAME,
        abund = VALUE,
        lev = "alpha",
        q = 2),
      
      beta_q2 = fn.divpart.long(
        site.id.vect = SITE_ID,
        spp = VARIABLE_NAME,
        abund = VALUE,
        lev = "beta",
        q = 2),
      
      gamma_q2 = fn.divpart.long(
        site.id.vect = SITE_ID,
        spp = VARIABLE_NAME,
        abund = VALUE,
        lev = "gamma",
        q = 2),
      
      # ELEMENTS OF METACOMMUNITY STRUCTURE

      # ems.struct = fn.ems.long(
      #   site.id.vect = SITE_ID,
      #   spp = VARIABLE_NAME,
      #   abund = VALUE,
      #   method = "r1",
      #   sims = 10)
    )
  print(mc.analysis.by.timestep)
}
  
# 
#   
#   rows.to.add <- as.data.frame(matrix(nrow = 0, ncol = ncol(mc.data)))
#   colnames(rows.to.add) <- colnames(mc.data)
#   for(each.date in unique(mc.data$DATE)){
#     new.spatial.rows <- tbl_df(filter(mc.data, OBSERVATION_TYPE == "SPATIAL_COORDINATE"))
#     new.spatial.rows$DATE <- rep(each.date, dim(new.spatial.rows)[1])
#     rows.to.add <- bind_rows(rows.to.add, new.spatial.rows)
#   }
#   mc.data <- bind_rows(mc.data, rows.to.add)
#   
#   mc.data %>% group_by(DATE) %>%
#     summarise(
#       
#       var.part = fn.varpart.long(
#         OBSERVATION_TYPE = OBSERVATION_TYPE,
#         SITE_ID = SITE_ID,
#         VARIABLE_NAME = VARIABLE_NAME,
#         VALUE = VALUE
#       )
#       
#     )
# 
#   
# Call the function

fn.mc.analyze(mc.data = sbc.data)


