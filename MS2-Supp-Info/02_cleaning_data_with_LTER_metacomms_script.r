#' ################################################
#' Data cleaning script
#' 
#' Last updated -- Eric Sokol -- 12 June 2019
#' 
#' based on script by
#'   Thomas Lamy
#' ###############################################

# Clean out workspace
rm(list = ls())
gc()

# required libraries
library(tidyverse)
library(reshape2)

# devtools::install_github('sokole/ltermetacommunities/ltmc')
library(ltmc)

# source Thomas's function for comparison against ltmc::metacommunity_variability,
# they should return the same results
source('space_stab.R')

# files downloaded in script 01
dt1 <- read_csv('RAW_DOWNLOAD_2019-06-12.csv')

######################################################
# some basic data munging
##############################

# recode taxon with "NA" code, this could be problematic
dt1$SP_CODE[dt1$SCIENTIFIC_NAME == 'Nienburgia andersoniana'] <- 'NANDERSON'

# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)

# Keep the new dates only if they all converted correctly
if(length(tmp1DATE) == length(tmp1DATE[!is.na(tmp1DATE)])){
  dt1$DATE <- tmp1DATE 
}else{
  print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")
}
rm(tmpDateFormat,tmp1DATE) 

# get YEAR from DATE
dt1$YEAR <- as.numeric(format(dt1$DATE, '%Y'))

# rename data.frame
sbc.data.transect <- dt1
sbc.data.transect$SU <- as.factor(paste(sbc.data.transect$SITE, sbc.data.transect$TRANSECT, sep="_"))
rm(dt1)

#########################################
# Data cleaning -- from Thomas Lamy
#########################

## transect to remove
transects_rm <- c("IVEE_3", "IVEE_4", "IVEE_5", "IVEE_6", "IVEE_7", "IVEE_8", "SCDI_1", "SCTW_1",
                  "AHND_1", "ABUR_2", "NAPL_4")

## Changing -99999 by 0 (assuming that the species was not present)
sbc.data.transect[sbc.data.transect$SFDM == -99999,]$SFDM <- 0     # 2.7% -99999
sbc.data.sessile <- sbc.data.transect %>%
  filter(YEAR %in% c(2004:2017)) %>% 
  filter(COARSE_GROUPING %in% c("SESSILE INVERT", "UNDERSTORY ALGAE")) %>% 
  filter(!(SP_CODE %in% c("BLD", "DIAT", "UBB", "UEC", "ZOMA"))) %>%        # non-algae species
  filter(!(SP_CODE %in% c("PACA", "CHOV", "PU"))) %>%                       # rock boring clams
  filter(!(SU %in% transects_rm)) %>% 
  ## biomass data at the site level, data has been zero-filled
  group_by(YEAR, SITE, SP_CODE, SCIENTIFIC_NAME, COARSE_GROUPING) %>%
  summarize(VALUE=mean(SFDM)) %>% 
  rename(DATE=YEAR, SITE_ID=SITE, VARIABLE_NAME=SP_CODE) %>%
  ungroup() %>% as.data.frame() %>% droplevels

write_csv(sbc.data.sessile, 'DATA_sbc.data.sessile.long.csv')

# subset(sbc.data.sessile, DATE==2017 & SITE_ID=="AHND") # only one species ... 

#### wide community matrix ####
sbc.wide.sessile <- reshape2::dcast(sbc.data.sessile, SITE_ID + DATE ~ VARIABLE_NAME, value.var = "VALUE")
# create blocks corresponding to local communities
sbc.wide.sessile <- sbc.wide.sessile[order(sbc.wide.sessile$DATE),]
sbc.wide.sessile <- sbc.wide.sessile[order(sbc.wide.sessile$SITE_ID),]

write_csv(sbc.wide.sessile, 'DATA_sbc.data.sessile.csv')

# number of local communities (s)
s <- length(unique(sbc.wide.sessile$SITE_ID))
# number of sampled years (t)
t <- length(unique(sbc.wide.sessile$DATE))
# community matrix (Y)
Y <- sbc.wide.sessile[,3:dim(sbc.wide.sessile)[2]]
# removing species never sampled
which(apply(Y, 2, sum) == 0)
# PHTO SELO 
# 89  109 
Y <- Y[,which(apply(Y,2, sum) != 0)]   # 124 species / 122 species


# calc total community biomass for each local community for each year
tot.bio <- apply(Y, 1, sum)
plot.tot.bio.local <- cbind(sbc.wide.sessile[,c("SITE_ID", "DATE")], tot.bio)

# write out table
write_csv(plot.tot.bio.local, 'DATA_plot.tot.bio.local.csv')

# calc total regional biomass for each year
plot.tot.bio.regional <- plot.tot.bio.local %>%
  group_by(DATE) %>%
  summarize(tot.bio = sum(tot.bio)) %>%
  ungroup() %>% as.data.frame() 

# write out table
write_csv(plot.tot.bio.regional, 'DATA_plot.tot.bio.regional.csv')

########################################################
#### Compute metacommunity stability ####
##############################################

# use Thomas's function
res <- space_stab(Y, s, t)
mult <- reshape2::melt(res[,1:9])

# use ltmc function --  should give the same results
my_res_TBD <- ltmc::metacommunity_variability(
  data_wide = sbc.wide.sessile,
  time_step_col_name = 'DATE',
  site_id_col_name = 'SITE_ID',
  taxon_list = names(sbc.wide.sessile)[
    !names(sbc.wide.sessile)%in%c('DATE','SITE_ID')],
  variability_type = 'comm',
  standardization_method = 'hT')

my_res_HBD <- ltmc::metacommunity_variability(
  data_wide = sbc.wide.sessile,
  time_step_col_name = 'DATE',
  site_id_col_name = 'SITE_ID',
  taxon_list = names(sbc.wide.sessile)[
    !names(sbc.wide.sessile)%in%c('DATE','SITE_ID')],
  variability_type = 'comm',
  standardization_method = 'h')

my_res_CV <- ltmc::metacommunity_variability(
  data_wide = sbc.wide.sessile,
  time_step_col_name = 'DATE',
  site_id_col_name = 'SITE_ID',
  taxon_list = names(sbc.wide.sessile)[
    !names(sbc.wide.sessile)%in%c('DATE','SITE_ID')],
  variability_type = 'agg')

my_mult <- data.frame(
  variable = c('GammaCV','AlphaCV','PhiCV',
               'GammaTBD','AlphaTBD','PhiTBD',
               'GammaHBD','AlphaHBD','PhiHBD'),
  value = c(
    unlist(my_res_CV[c('gamma_var','alpha_var','phi_var')]),
    unlist(my_res_TBD[c('gamma_var','alpha_var','phi_var')]),
    unlist(my_res_HBD[c('gamma_var','alpha_var','phi_var')])))


# write out results from comparison
write_csv(mult, 'RESULTS_metacomm_var__space_stab.csv')
write_csv(my_mult, 'RESULTS_metacomm_var__ltmc.csv')
