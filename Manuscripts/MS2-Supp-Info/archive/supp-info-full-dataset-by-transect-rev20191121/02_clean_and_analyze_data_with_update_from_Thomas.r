## ---------------------------- ## 
## Data loading                 ##
## ---------------------------- ## 
## SBC LTER data (download data: 2018/06/08 - from Li)
# data.long <- read.csv(paste(my_wd, "Data/20180601/Annual_All_Species_Biomass_at_transect.csv", sep=""), header=TRUE)

#############################
#############################
# Eric's code to read in data that was downloaded from portal in step 1
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

# file downloaded in script 01 -- pick the first one if there were repeat downloads.
my_local_file_list <- list.files()
my_raw_data_file <- my_local_file_list %>% 
  .[grep('RAW_DOWNLOAD_', .)] %>% .[1]
dt1 <- read_csv(my_raw_data_file)

# recode taxon with "NA" code, this could be problematic
dt1$SP_CODE[dt1$SCIENTIFIC_NAME == 'Nienburgia andersoniana'] <- 'NANDERSON'

# paste into data.frame to go into Thomas's cleaning code
# rename fields to match Thomas's names
data.long <- dt1 %>%
  rename(DRY_GM2 = DM_GM2) %>%
  as.data.frame()

#############################
#############################


## cheking for NA
data.long[is.na(data.long$DRY_GM2),]  # 0
## cheking -99999 -- these are different for Eric
dim(data.long[data.long$WM_GM2 == -99999,])[1]/dim(data.long)[1]*100  # 3.143276
dim(data.long[data.long$DRY_GM2 == -99999,])[1]/dim(data.long)[1]*100 # 3.143276
dim(data.long[data.long$SFDM == -99999,])[1]/dim(data.long)[1]*100    # 29.78665
dim(data.long[data.long$AFDM == -99999,])[1]/dim(data.long)[1]*100    # 5.718074
## Changing -99999 by 0 (assuming that the species was not present)
data.long[data.long$WM_GM2 == -99999,]$WM_GM2 <- 0 
data.long[data.long$DRY_GM2 == -99999,]$DRY_GM2 <- 0 
data.long[data.long$SFDM == -99999,]$SFDM <- 0 
data.long[data.long$AFDM == -99999,]$AFDM <- 0

## create a plot id = spatial unit (SU)
data.long$SU <- as.factor(paste(data.long$SITE, data.long$TRANSECT, sep="_"))
unique(data.long$SU); length(unique(data.long$SU))  # N = 51 plots but 12 are not consistantly surveyed
# Eric get's 44 plots

table(data.long$SU, data.long$YEAR)

## transect to remove
transects_rm <- c("IVEE_3", "IVEE_4", "IVEE_5", "IVEE_6", "IVEE_7", "IVEE_8", "SCDI_1", "SCTW_1")
## transect to remove (sandy transects)
# transects_rm <- c("IVEE_3", "IVEE_4", "IVEE_5", "IVEE_6", "IVEE_7", "IVEE_8", "SCDI_1", "SCTW_1",
#                   "AHND_1", "ABUR_2", "NAPL_4")
## year selection
year_sel <- c(2004:2017)

## ---------------------------- ## 
## Datasets long format         ##
## ---------------------------- ## 
## Understory macroalgae
data.long.algae <- data.long %>%
  filter(YEAR %in% year_sel) %>% 
  filter(!(SU %in% transects_rm)) %>% 
  filter(!(SP_CODE %in% c("BLD", "DIAT", "UBB", "UEC", "ZOMA"))) %>% 
  filter(COARSE_GROUPING == "UNDERSTORY ALGAE") %>% 
  ungroup() %>% droplevels

## spatial structure of the data
table(data.long.algae$SITE, data.long.algae$SU)
## 11 sites/reefs, 39 plots/transects, 2 to 8 plots/reefs
# ABUR = 2
# AHND = 2
# AQUE = 6
# BULL = 3
# CARP = 8
# GOLB = 2
# IVEE = 2
# MOHK = 2
# NAPL = 8
# SCDI = 2
# SCTW = 2

# write cleaned data locally
# write_csv(data.long.algae, 'CLEANED_data_long_algae.csv')

##################################
sampling_design <- data.long.algae %>%
  select(YEAR, MONTH, DATE, SU) %>% 
  distinct()

# View(sampling_design)

# # identify and remove transects that have years with zero biomass -- analysis won't work
# site_by_year_summary <- data.long.algae %>% 
#   group_by(SU, YEAR) %>% 
#   summarize(TOT_SFDM = sum(SFDM))
# 
# site_years_with_0_biomass <- site_by_year_summary %>%
#   filter(TOT_SFDM == 0)
# 
# data.long.algae <- data.long.algae %>%
#   filter(!SU %in% site_years_with_0_biomass$SU)

# each SU has one survey per year

##################################

# extract columns from data needed for this analysis 
# rename to play nice with canned code
data.long.analysis <- data.long.algae %>%
  select(SU, YEAR, SP_CODE, SFDM) %>%
  rename(DATE = YEAR,
         SITE_ID = SU,
         VALUE= SFDM)

# identify species never sampled
spp_summary <- data.long.analysis %>%
  group_by(SP_CODE) %>%
  summarize(frequency = sum(VALUE > 0))
  
spp_to_remove <- spp_summary %>%
  filter(frequency == 0) 

# remove unobserved taxon code from analysis
data.long.analysis <- data.long.analysis %>%
  filter(!SP_CODE %in% spp_to_remove$SP_CODE)

# get taxon code list
spp_list <- data.long.analysis$SP_CODE %>% unique()

# make a wide data frame -- for Thomas's code
data.wide.analysis <- data.long.analysis %>%
  spread(SP_CODE, VALUE, fill = 0)

# write cleaned data
write_csv(data.wide.analysis, 'DATA_wide.csv')
write_csv(data.long.analysis, 'DATA_long.csv')

##################################
# get vars for Thomas's code

# number of local communities (s)
s <- length(unique(data.wide.analysis$SITE_ID))
# number of sampled years (t)
t <- length(unique(data.wide.analysis$DATE))
# community matrix (Y)
Y <- data.wide.analysis[,spp_list]

# calc total community biomass for each local community for each year
tot.bio <- apply(Y, 1, sum)

# make data frame for plotting with renamed variables to play nice with plotting code
plot.tot.bio.local <- cbind(data.wide.analysis[,c("SITE_ID", "DATE")], tot.bio) 

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
  data_wide = data.wide.analysis,
  time_step_col_name = 'DATE',
  site_id_col_name = 'SITE_ID',
  taxon_list = names(data.wide.analysis)[
    !names(data.wide.analysis)%in%c('DATE','SITE_ID')],
  variability_type = 'comm',
  standardization_method = 'hT')

my_res_HBD <- ltmc::metacommunity_variability(
  data_wide = data.wide.analysis,
  time_step_col_name = 'DATE',
  site_id_col_name = 'SITE_ID',
  taxon_list = names(data.wide.analysis)[
    !names(data.wide.analysis)%in%c('DATE','SITE_ID')],
  variability_type = 'comm',
  standardization_method = 'h')

my_res_CV <- ltmc::metacommunity_variability(
  data_wide = data.wide.analysis,
  time_step_col_name = 'DATE',
  site_id_col_name = 'SITE_ID',
  taxon_list = names(data.wide.analysis)[
    !names(data.wide.analysis)%in%c('DATE','SITE_ID')],
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
