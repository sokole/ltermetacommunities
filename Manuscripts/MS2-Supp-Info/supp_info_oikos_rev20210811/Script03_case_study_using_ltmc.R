####################################################################################
## Case study to illustrate the dual nature of metacommunity variability          ##
## SBC LTER: understory macroalgal communities                                    ##
####################################################################################
## Thomas Lamy, April 18th 2017
## updated by Eric Sokol, Aug 12 2021

#### setting R ####
# load packages
library(tidyverse)
library(reshape2)


## load function space_stab to compute compositional and aggregate stability across spatial scales
source("space_stab.R")


#### load data ####
## load Package ID: knb-lter-sbc.50.7 as R object sbc.data
source("SBC_LTER_data.R")


#### Format data ####
## create a unique ID for each transect
sbc.data$SU <- as.factor(paste(sbc.data$SITE, sbc.data$TRANSECT, sep="_"))
## transect to remove (e.g. sandy transects)
transects_rm <- c("IVEE_3", "IVEE_4", "IVEE_5", "IVEE_6", "IVEE_7", "IVEE_8", "SCDI_1", "SCTW_1",
                  "AHND_1", "ABUR_2", "NAPL_4")
## Changing NA by 0 (assuming that the species was not present)
sbc.data[is.na(sbc.data$SFDM),]$SFDM <- 0     # 2.8% of NA
## Filter and merge at the site level
sbc.data.algae <- sbc.data %>%
  filter(YEAR %in% c(2004:2017)) %>%
  filter(COARSE_GROUPING == "UNDERSTORY ALGAE") %>%
  filter(!(SP_CODE %in% c("BLD", "DIAT", "UBB", "UEC", "ZOMA"))) %>% # non-algae species
  filter(!(SU %in% transects_rm)) %>%
  ## biomass data at the site level, data has been zero-filled
  group_by(YEAR, SITE, SP_CODE, SCIENTIFIC_NAME, COARSE_GROUPING) %>%
  summarize(VALUE=mean(SFDM)) %>%
  rename(DATE=YEAR, SITE_ID=SITE, VARIABLE_NAME=SP_CODE) %>%
  ungroup() %>% as.data.frame() %>% droplevels

## number of transects per site
nb.su <- sbc.data %>%
  filter(YEAR %in% c(2004:2017)) %>%
  filter(COARSE_GROUPING == "UNDERSTORY ALGAE") %>%
  filter(!(SP_CODE %in% c("BLD", "DIAT", "UBB", "UEC", "ZOMA"))) %>% # non-algae species
  filter(!(SU %in% transects_rm)) %>%
  ungroup() %>% as.data.frame() %>% droplevels
length(levels(nb.su$SU)) # 36
with(nb.su, table(SU, SITE))

#### wide community matrix ####
sbc.wide.algae <- dcast(sbc.data.algae, SITE_ID + DATE ~ VARIABLE_NAME, value.var="VALUE")
# create blocks corresponding to local communities
sbc.wide.algae <- sbc.wide.algae[order(sbc.wide.algae$DATE),]
sbc.wide.algae <- sbc.wide.algae[order(sbc.wide.algae$SITE_ID),]
# number of local communities (s)
s <- length(unique(sbc.wide.algae$SITE_ID))
# number of sampled years (t)
t <- length(unique(sbc.wide.algae$DATE))
# community matrix (Y)
Y <- sbc.wide.algae[,3:dim(sbc.wide.algae)[2]]
# removing species never sampled
which(apply(Y, 2, sum) == 0)
# PHTO SELO
# 40   51
Y <- Y[,which(apply(Y,2, sum) != 0)]   # 55 species

#### Compute metacommunity variability ####

# using space_stab
res <- space_stab(Y, s, t)
print(res)

# using ltmc
# use devtools to install package from GitHub repo
# devtools::install_github("sokole/ltermetacommunities/ltmc")
library(ltmc)

ltmc::metacommunity_variability(
  data_wide = sbc.wide.algae,
  site_id_col_name = "SITE_ID",
  time_step_col_name = "DATE",
  variability_type = "agg")

ltmc::metacommunity_variability(
  data_wide = sbc.wide.algae,
  site_id_col_name = "SITE_ID",
  time_step_col_name = "DATE",
  variability_type = "comm")




## End
