setwd("C:/Users/Aldo/MEGA/Projects/LTER/WK_group/data")
library(dplyr)
library(tidyr)
library(testthat)

# FCE env data ------------------------
fec_env      <- read.csv("FCE_env.csv", stringsAsFactors = F)
fec_diat     <- read.csv("FCE_diatoms.csv", stringsAsFactors = F)
fec_algae    <- read.csv("FCE_algae.csv", stringsAsFactors = F)

# format diatom data
vars <- c("OBSERVATION_TYPE","SITE_ID","DATE",
          "VARIABLE_NAME","VARIABLE_UNITS","VALUE")

# coordinates ----------------------------------------------------------------
# find a centroid for each PSU
centr   <- fec_env %>%
            mutate(PSU = as.factor(PSU) ) %>%
            select(PSU, EASTING, NORTHING) %>%
            group_by(PSU) %>%
            summarise( east_centroid = mean(EASTING),
                       north_centroid = mean(NORTHING) )

# check we didn't loose the PSU values (through "as.factor")
expect_equal(length(intersect(unique(as.character(fec_env$PSU)),
                       as.character(coord$PSU))),
             length(unique(fec_env$PSU)))


# coordinates data frame
coord <- data.frame( OBSERVATION_TYPE = "SPATIAL_COORDINATE",
                      SITE_ID = as.character(centr$PSU),
                      DATE = NA,
                      VARIABLE_NAME = "EASTING",
                      VARIABLE_UNITS = "METERS",
                      VALUE = centr$east_centroid) %>%
            rbind( data.frame( OBSERVATION_TYPE = "SPATIAL_COORDINATE",
                               SITE_ID = as.character(centr$PSU),
                               DATE = NA,
                               VARIABLE_NAME = "NORTHING",
                               VARIABLE_UNITS = "METERS",
                               VALUE = centr$north_centroid)
            )


# taxon count ----------------------------------------------------------

# calculate mean of diatom species across samples
# (NOT NEEDED, number of rows does not change)
mean_diat <- fec_diat %>% 
              select(Year, PSU, ACBREBRE:UNKNVALV) %>%
              group_by(Year, PSU) %>%
              summarise_each(funs(mean))
# test equality of two data sets
expect_equal(nrow(mean_diat), nrow(fec_diat) )

# long-form this data set
diat_long <- fec_diat %>%
              select(Year, PSU, ACBREBRE:UNKNVALV) %>%
              gather(VARIABLE_NAME, VALUE,-Year,-PSU) %>%
              mutate(Year = Year + 2000,
                     VARIABLE_UNITS = "PERCENT",
                     OBSERVATION_TYPE = "TAXON_RELATIVE_ABUNDANCE") %>%
              rename(DATE = Year,
                     SITE_ID = PSU) %>%
              select_(.dots = vars)
  

# environmental variables -----------------------------------------------------

# convert from character to numeric
env_mat   <- fec_env %>%
              select(Depth_cm:Chl.a..µg.m2.) %>%
              cbind( select(fec_env, PLT_COVER:HYDROPERIOD_MEAN) )

# identify character columns
classes   <- env_mat %>%
              lapply(class) %>%
              unlist() %>%
              as.character()

# grep variables which are columns
char_id   <- which(classes == "character")

# manually inspect each of 22 vars
unique(env_mat[,char_id[22]])

# introduce NAs in character columsn
# PLUS, remove ","
for(i in seq_along(char_id)){
  
  # introduce NAs
  env_mat[,char_id[i]] <- gsub("NS", "NA", env_mat[,char_id[i]])
  env_mat[,char_id[i]] <- gsub("ns", "NA", env_mat[,char_id[i]])
  env_mat[,char_id[i]] <- gsub("9999", "NA", env_mat[,char_id[i]])
  env_mat[,char_id[i]] <- gsub("8888", "NA", env_mat[,char_id[i]])
  env_mat[,char_id[i]] <- gsub("7777", "NA", env_mat[,char_id[i]])
  env_mat[,char_id[i]] <- gsub("6666", "NA", env_mat[,char_id[i]])
  env_mat[,char_id[i]] <- gsub("Z", "NA", env_mat[,char_id[i]])
  env_mat[,char_id[i]] <- gsub("UP", "NA", env_mat[,char_id[i]])
  
  # remove commas
  env_mat[,char_id[i]] <-gsub(",", "", env_mat[,char_id[i]])
  
  # convert to numeric
  env_mat[,char_id[i]] <- as.numeric( env_mat[,char_id[i]] )
  
}

# Remove non-numeric variables
non_num_vars <- c("Sample.Quantitative...Y.or.N.",
                  "Plant.Sp1","Plant.Sp2","Plant.Sp3",
                  "Floating.Sp1","Floating.Sp2","Green.Sp1")
env_n_mat    <- env_mat[,-which(names(env_mat) %in% non_num_vars)]

# introduce NAs in numeric fields
for(i in 1:ncol(env_n_mat)){

  env_n_mat[env_n_mat[,i]==9999 & 
            !is.na(env_n_mat[,i]), i] <- NA
  
}
 
# means across replicates
mean_env <- fec_env %>%
              select(Year, PSU) %>%
              cbind(env_n_mat) %>%
              group_by(Year, PSU) %>%
              summarise_each(funs(mean))

# Environment in long form
env_long  <- mean_env %>%
              as.data.frame() %>%
              gather(VARIABLE_NAME, VALUE,-Year,-PSU) %>%
              mutate(Year = Year + 2000,
                     VARIABLE_UNITS = "UNIT",
                     OBSERVATION_TYPE = "ENV_VAR") %>%
              rename(DATE = Year,
                     SITE_ID = PSU) %>%
              select_(.dots = vars)


# put it all together! -------------------------------------------------------------------
data_set <- Reduce(function(...) rbind(...),
                   list(coord, diat_long, env_long))

write.csv(data_set, "FCE_diatoms_long.csv", 
          row.names = F)

#write.csv(names(mean_env)[-c(1:2)], "fec_env_var_names.csv",
#          row.names=F)

