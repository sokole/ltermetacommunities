setwd("C:/Users/Aldo/MEGA/Projects/LTER/WK_group/data")
library(dplyr)
library(tidyr)

# pop data
fish  <- read.csv("NTL_Fish.csv", stringsAsFactors = F)
macr  <- read.csv("NTL_Macroinvertebrates.csv", stringsAsFactors = F)
zoo   <- read.csv("NTL_Zooplankton.csv", stringsAsFactors = F)

# env data
chem  <- read.csv("NTL_chemistry.csv", stringsAsFactors = F)
#ions  <- read.csv("NTL_ions.csv",      stringsAsFactors = F)

# coord
latlon <- read.csv("NTL_lake_coord.csv", stringsAsFactors = F)

# long data frame variables
vars <- c("OBSERVATION_TYPE","SITE_ID","DATE",
          "VARIABLE_NAME","VARIABLE_UNITS","VALUE")


# coordinates ------------------------------------------------
coord <- data.frame( OBSERVATION_TYPE = "SPATIAL_COORDINATE",
                     SITE_ID = latlon$lakeid,
                     DATE = NA,
                     VARIABLE_NAME = "LATITUDE",
                     VARIABLE_UNITS = "decimal.degrees",
                     VALUE = latlon$Latitude) %>%
  rbind( data.frame( OBSERVATION_TYPE = "SPATIAL_COORDINATE",
                     SITE_ID = latlon$lakeid,
                     DATE = NA,
                     VARIABLE_NAME = "LONGITUDE",
                     VARIABLE_UNITS = "decimal.degrees",
                     VALUE = latlon$Longitude)
  )


# environmental data ---------------------------------------

# select ph and p (phosphorous) data
e_pph   <- chem %>% 
            select(lakeid, year4, sampledate, ph, totpf, totpuf) 

# split sampledate fields and make them numeric
list_da <- e_pph$sampledate %>%
            strsplit("-") %>%
            unlist() %>% 
            matrix(nrow(e_pph),3,byrow=T) %>%
            as.data.frame(stringsAsFactors = F) %>%
            lapply(function(x) x = as.numeric(x)) 
           
# put together date vars in same data frame
date_mat <- Reduce(function(...) cbind(...), list_da) %>%
              as.data.frame() %>%
              setNames(c("year", "month", "day"))  

# to compute MEANS, Sept to Dec belong to PREVIOUS YEAR 
m_id                <- date_mat$month>8
date_mat$year[m_id] <- date_mat$year[m_id]-1

# add date data frame to env 
env_pph  <- date_mat %>% 
              cbind(e_pph) %>%
              select(-sampledate,-year4) # no need for these no more

# calculate means
env_avg  <- env_pph %>%
              select(-month,-day) %>%
              group_by(year, lakeid) %>%
              summarise_each(funs(mean(.,na.rm=T)))
  
# YEARLY environmental data, pre-unit of measures
env_pre  <- env_avg %>%
              gather(VARIABLE_NAME, VALUE,-year,-lakeid) %>%
              mutate(OBSERVATION_TYPE = "ENV_VAR") %>%
              rename(DATE = year,
                     SITE_ID = lakeid)

# define variable units of measure
units    <- data.frame(VARIABLE_NAME = unique(env_pre$VARIABLE_NAME),
                       VARIABLE_UNITS = c("pH", "microgramsPerLiter","microgramsPerLiter")                  
                       )

# environmental data in long form
env_long <- units %>% 
              merge(env_pre)


# Population data (marcoinvertebrates) ----------------------------------------------------

# aggregate data at the lake level, across depths, and readings
macr_avg <- macr %>%
              group_by(lakeid, year4, taxon) %>%
              summarise(TAXON_COUNT = mean(number_indiv, na.rm=T))

# macroinvertebrate data in long form
macr_long <- macr_avg %>%
              select(year4, lakeid, TAXON_COUNT, taxon) %>%
              gather(VARIABLE_NAME, VALUE,-year4,-lakeid) %>%
              mutate(VARIABLE_UNITS = "NUMBER",
                     OBSERVATION_TYPE = "AVERAGE_COUNTS") %>%
              setNames(c("DATE","SITE_ID","VARIABLE_NAME", "VALUE", 
                         "VARIABLE_UNITS", "OBSERVATION_TYPE")) %>%
              as.data.frame() %>%
              select_(.dots = vars)


# put it all together! -------------------------------------------------------------------
data_set <- Reduce(function(...) rbind(...),
                   list(coord, macr_long, env_long))

write.csv(data_set, "NTL_Macroinvertebrates_long.csv", 
          row.names = F)


# Population data (zooplankton) ----------------------------------------------------

# get date
date_z   <- zoo$sample_date %>%
              strsplit(" ") %>%
              unlist() %>%
              matrix(nrow(zoo),2,byrow=T) %>%
              as.data.frame() %>%
              setNames(c("date","time")) %>%
              select(date)

# get month/day/year columns
date_col <- date_z[,1] %>%
              as.character() %>%
              strsplit("/") %>%
              unlist() %>%
              matrix(nrow(zoo),3,byrow=T) %>%
              as.data.frame() %>%
              setNames(c("month","day","year"))
