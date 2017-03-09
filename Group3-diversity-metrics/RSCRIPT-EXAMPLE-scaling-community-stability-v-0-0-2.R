#########################
# libraries
#########################
library(tidyverse)

source("RFUNC-beta.div.comp.R")
source("RFUNC-LCBD.comp.R")
library(ade4)
library(vegan)


#########################
# read data
#########################
# <<read via google id here>>

dat.in.long <- read.csv('Y1.csv')

#########################
# user specified variables
#########################
location_name <- 'site'
time_step_name <- 'time'
taxon_name <- 'variable'
taxon_count_name <- 'value'

#########################
# user specified variables
#########################

taxon.list <- dat.in.long[,taxon_name] %>% as.character() %>% unique()
dat.in.wide.spp <- dat.in.long[,c(location_name,
                                  time_step_name,
                                  taxon_name,
                                  taxon_count_name)] %>% tidyr::spread(key = variable, value = value)

######################################################
######################################################
######################################################
######################################################
# functions
#########################

# -- total beta-diversity
fn_bd_total <- function(
  Y, #site by species counts or RAs
  ...
){
  D <- vegan::vegdist(Y, ...)
  D_mat <- as.matrix(D)
  D_lower_tri <- D_mat[lower.tri(D_mat, diag = FALSE)]
  n_obs <- nrow(D_mat)
  return(sum(D_lower_tri) / (n_obs * (n_obs -1)))
}

# -- cumulative path-length beta-diversity
fn_bd_cum_path <- function(
  Y, #site by species counts or RAs
  order_vector = 1:nrow(Y),
  ...
){
  D <- as.matrix(vegan::vegdist(Y[order_vector,], ...))
  D_mat_sequential_obs <- diag(D[-1,-ncol(D)]) #sub-diagonal
  cumulative_path_bd <- sum(D_mat_sequential_obs)
  return(cumulative_path_bd)
}

# -- average inter-annual turnover "rate"
fn_bd_mean_turnover_rate <- function(
  Y, #site by species counts or RAs
  order_vector = 1:nrow(Y),
  ...
){
  D <- as.matrix(vegan::vegdist(Y[order_vector,], ...))
  D_mat_sequential_obs <- diag(D[-1,-ncol(D)]) #sub-diagonal
  cumulative_path_bd <- sum(D_mat_sequential_obs)
  return(cumulative_path_bd/(nrow(Y)-1))
}

# -- bd component SS for all pairwise observations
fn_bd_components <- function(
  Y, #site by species counts or RAs
  bd_component_name = NULL, #options are 'D' 'repl' or 'rich', default is 'D'
  coef = "S",
  quant = TRUE,
  ...
){
  # source('RFUNC-beta.div.comp.R')
  # requires beta.div.comp -- assume this is in namespace. 
  # Will be in package as internal function.
  bd_all <- beta.div.comp(Y, coef = coef, quant = quant)
  
  if(is.null(bd_component_name)){
    D <- bd_all$D
  } else {
    D <- bd_all[[bd_component_name]]
  }
  
  D_mat <- as.matrix(D)
  D_lower_tri <- D_mat[lower.tri(D_mat, diag = FALSE)]
  n_obs <- nrow(D_mat)
  return(sum(D_lower_tri) / (n_obs * (n_obs -1)))
}


# -- cumulative path-length beta-diversity compnents
fn_bd_components_cum_path <- function(
  Y, #site by species counts or RAs
  order_vector = 1:nrow(Y),
  bd_component_name = NULL, #options are 'D' 'repl' or 'rich', default is 'D'
  coef = "S",
  quant = TRUE,
  ...
){
  # source('RFUNC-beta.div.comp.R')
  # requires beta.div.comp -- assume this is in namespace. 
  # Will be in package as internal function.
  bd_all <- beta.div.comp(Y, coef = coef, quant = quant)
  
  if(is.null(bd_component_name)){
    D <- bd_all$D
  } else {
    D <- bd_all[[bd_component_name]]
  }
  
  D_mat <- as.matrix(D)
  D_mat_sequential_obs <- diag(D_mat[-1,-ncol(D_mat)]) #sub-diagonal
  cumulative_path_bd <- sum(D_mat_sequential_obs)
  return(cumulative_path_bd)
}

fn_bd_components_mean_turnover_rate <- function(
  Y, #site by species counts or RAs
  order_vector = 1:nrow(Y),
  bd_component_name = NULL, #options are 'D' 'repl' or 'rich', default is 'D'
  coef = "S",
  quant = TRUE,
  ...
){
  # source('RFUNC-beta.div.comp.R')
  # requires beta.div.comp -- assume this is in namespace. 
  # Will be in package as internal function.
  bd_all <- beta.div.comp(Y, coef = coef, quant = quant)
  
  if(is.null(bd_component_name)){
    D <- bd_all$D
  } else {
    D <- bd_all[[bd_component_name]]
  }
  
  D_mat <- as.matrix(D)
  D_mat_sequential_obs <- diag(D_mat[-1,-ncol(D_mat)]) #sub-diagonal
  cumulative_path_bd <- sum(D_mat_sequential_obs)
  return(cumulative_path_bd/(nrow(Y)-1))
}

######################################################
######################################################
######################################################
######################################################

################
# -- total BD
################
# 1 -- temporal beta-div at each site
alpha_temporal_bd <- dat.in.wide.spp %>% 
  group_by(site) %>% 
  do(data.frame(bd_time = fn_bd_total(.[,taxon.list])))

# 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
dat.in.regional.means <- dat.in.wide.spp %>% group_by(time) %>%
  select(one_of(taxon.list)) %>%
  summarise_each(funs(mean))
gamma_temporal_bd <- fn_bd_total(dat.in.regional.means[,taxon.list])

# 3 -- 
mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time)

# 4 -- 
phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd

dat.bd.total <- data.frame(
  beta_type = 'total',
  gamma_temporal_bd,
  mean_alpha_temporal_bd,
  phi_bd,
  n_locations = length(unique(dat.in.wide.spp$site)),
  n_times = length(unique(dat.in.wide.spp$time)))

################
# -- total BD version 2
################
# 1 -- temporal beta-div at each site
alpha_temporal_bd <- dat.in.wide.spp %>% 
  group_by(site) %>% 
  do(data.frame(bd_time = fn_bd_components(.[,taxon.list])))

# 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
dat.in.regional.means <- dat.in.wide.spp %>% group_by(time) %>%
  select(one_of(taxon.list)) %>%
  summarise_each(funs(mean))
gamma_temporal_bd <- fn_bd_components(dat.in.regional.means[,taxon.list])

# 3 -- 
mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time)

# 4 -- 
phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd

dat.bd.total.v2 <- data.frame(
  beta_type = 'total_v2',
  gamma_temporal_bd,
  mean_alpha_temporal_bd,
  phi_bd,
  n_locations = length(unique(dat.in.wide.spp$site)),
  n_times = length(unique(dat.in.wide.spp$time)))

################
# -- total BD repl
################
# 1 -- temporal beta-div at each site
alpha_temporal_bd <- dat.in.wide.spp %>% 
  group_by(site) %>% 
  do(data.frame(bd_time = fn_bd_components(.[,taxon.list],
                                           bd_component_name = 'repl')))

# 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
dat.in.regional.means <- dat.in.wide.spp %>% group_by(time) %>%
  select(one_of(taxon.list)) %>%
  summarise_each(funs(mean))
gamma_temporal_bd <- fn_bd_components(dat.in.regional.means[,taxon.list],
                                      bd_component_name = 'repl')

# 3 -- 
mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time)

# 4 -- 
phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd

dat.bd.total.repl <- data.frame(
  beta_type = 'total_repl',
  gamma_temporal_bd,
  mean_alpha_temporal_bd,
  phi_bd,
  n_locations = length(unique(dat.in.wide.spp$site)),
  n_times = length(unique(dat.in.wide.spp$time)))

################
# -- total BD rich
################
# 1 -- temporal beta-div at each site
alpha_temporal_bd <- dat.in.wide.spp %>% 
  group_by(site) %>% 
  do(data.frame(bd_time = fn_bd_components(.[,taxon.list],
                                           bd_component_name = 'rich')))

# 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
dat.in.regional.means <- dat.in.wide.spp %>% group_by(time) %>%
  select(one_of(taxon.list)) %>%
  summarise_each(funs(mean))
gamma_temporal_bd <- fn_bd_components(dat.in.regional.means[,taxon.list],
                                      bd_component_name = 'rich')

# 3 -- 
mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time)

# 4 -- 
phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd

dat.bd.total.rich <- data.frame(
  beta_type = 'total_rich',
  gamma_temporal_bd,
  mean_alpha_temporal_bd,
  phi_bd,
  n_locations = length(unique(dat.in.wide.spp$site)),
  n_times = length(unique(dat.in.wide.spp$time)))

################
# -- cumulative path-length BD
################

# 1 -- temporal beta-div at each site
alpha_temporal_bd <- dat.in.wide.spp %>% 
  group_by(site) %>% 
  do(data.frame(bd_time = fn_bd_cum_path(.[,taxon.list])))

# 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
dat.in.regional.means <- dat.in.wide.spp %>% group_by(time) %>%
  select(one_of(taxon.list)) %>%
  summarise_each(funs(mean))
gamma_temporal_bd <- fn_bd_cum_path(dat.in.regional.means[,taxon.list])

# 3 -- 
mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time)

# 4 -- 
phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd

dat.bd.cum_path_length <- data.frame(
  beta_type = 'cum_path_length',
  gamma_temporal_bd,
  mean_alpha_temporal_bd,
  phi_bd,
  n_locations = length(unique(dat.in.wide.spp$site)),
  n_times = length(unique(dat.in.wide.spp$time)))

################
# -- cumulative path-length BD v2
################

# 1 -- temporal beta-div at each site
alpha_temporal_bd <- dat.in.wide.spp %>% 
  group_by(site) %>% 
  do(data.frame(bd_time = fn_bd_components_cum_path(.[,taxon.list], 
                                                    bd_component_name = 'D')))

# 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
dat.in.regional.means <- dat.in.wide.spp %>% group_by(time) %>%
  select(one_of(taxon.list)) %>%
  summarise_each(funs(mean))
gamma_temporal_bd <- fn_bd_components_cum_path(dat.in.regional.means[,taxon.list],
                                               bd_component_name = 'D')

# 3 -- 
mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time)

# 4 -- 
phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd

dat.bd.cum_path_length_v2 <- data.frame(
  beta_type = 'cum_path_length_v2',
  gamma_temporal_bd,
  mean_alpha_temporal_bd,
  phi_bd,
  n_locations = length(unique(dat.in.wide.spp$site)),
  n_times = length(unique(dat.in.wide.spp$time)))

################
# -- cumulative path-length BD repl
################

# 1 -- temporal beta-div at each site
alpha_temporal_bd <- dat.in.wide.spp %>% 
  group_by(site) %>% 
  do(data.frame(bd_time = fn_bd_components_cum_path(.[,taxon.list],
                                                    bd_component_name = 'repl')))

# 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
dat.in.regional.means <- dat.in.wide.spp %>% group_by(time) %>%
  select(one_of(taxon.list)) %>%
  summarise_each(funs(mean))
gamma_temporal_bd <- fn_bd_components_cum_path(dat.in.regional.means[,taxon.list], 
                                               bd_component_name = 'repl')

# 3 -- 
mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time)

# 4 -- 
phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd

dat.bd.cum_path_length_repl <- data.frame(
  beta_type = 'cum_path_length_repl',
  gamma_temporal_bd,
  mean_alpha_temporal_bd,
  phi_bd,
  n_locations = length(unique(dat.in.wide.spp$site)),
  n_times = length(unique(dat.in.wide.spp$time)))

################
# -- cumulative path-length BD repl
################

# 1 -- temporal beta-div at each site
alpha_temporal_bd <- dat.in.wide.spp %>% 
  group_by(site) %>% 
  do(data.frame(bd_time = fn_bd_components_cum_path(.[,taxon.list],
                                                    bd_component_name = 'rich')))

# 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
dat.in.regional.means <- dat.in.wide.spp %>% group_by(time) %>%
  select(one_of(taxon.list)) %>%
  summarise_each(funs(mean))
gamma_temporal_bd <- fn_bd_components_cum_path(dat.in.regional.means[,taxon.list], 
                                               bd_component_name = 'rich')

# 3 -- 
mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time)

# 4 -- 
phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd

dat.bd.cum_path_length_rich <- data.frame(
  beta_type = 'cum_path_length_rich',
  gamma_temporal_bd,
  mean_alpha_temporal_bd,
  phi_bd,
  n_locations = length(unique(dat.in.wide.spp$site)),
  n_times = length(unique(dat.in.wide.spp$time)))

################
# -- turnover "rate"
################

# 1 -- temporal beta-div at each site
alpha_temporal_bd <- dat.in.wide.spp %>% 
  group_by(site) %>% 
  do(data.frame(bd_time = fn_bd_mean_turnover_rate(.[,taxon.list])))

# 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
dat.in.regional.means <- dat.in.wide.spp %>% group_by(time) %>%
  select(one_of(taxon.list)) %>%
  summarise_each(funs(mean))
gamma_temporal_bd <- fn_bd_mean_turnover_rate(dat.in.regional.means[,taxon.list])

# 3 -- 
mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time)

# 4 -- 
phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd

dat.bd.turnover_rate <- data.frame(
  beta_type = 'turnover_rate',
  gamma_temporal_bd,
  mean_alpha_temporal_bd,
  phi_bd,
  n_locations = length(unique(dat.in.wide.spp$site)),
  n_times = length(unique(dat.in.wide.spp$time)))

################
# -- turnover "rate" v2
################

# 1 -- temporal beta-div at each site
alpha_temporal_bd <- dat.in.wide.spp %>% 
  group_by(site) %>% 
  do(data.frame(bd_time = fn_bd_components_mean_turnover_rate(.[,taxon.list],
                                                              bd_component_name = 'D')))

# 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
dat.in.regional.means <- dat.in.wide.spp %>% group_by(time) %>%
  select(one_of(taxon.list)) %>%
  summarise_each(funs(mean))
gamma_temporal_bd <- fn_bd_components_mean_turnover_rate(
  dat.in.regional.means[,taxon.list],
  bd_component_name = 'D')

# 3 -- 
mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time)

# 4 -- 
phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd

dat.bd.turnover_rate_v2 <- data.frame(
  beta_type = 'turnover_rate_v2',
  gamma_temporal_bd,
  mean_alpha_temporal_bd,
  phi_bd,
  n_locations = length(unique(dat.in.wide.spp$site)),
  n_times = length(unique(dat.in.wide.spp$time)))


################
# -- turnover "rate" repl
################

# 1 -- temporal beta-div at each site
alpha_temporal_bd <- dat.in.wide.spp %>% 
  group_by(site) %>% 
  do(data.frame(bd_time = fn_bd_components_mean_turnover_rate(.[,taxon.list],
                                                              bd_component_name = 'repl')))

# 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
dat.in.regional.means <- dat.in.wide.spp %>% group_by(time) %>%
  select(one_of(taxon.list)) %>%
  summarise_each(funs(mean))
gamma_temporal_bd <- fn_bd_components_mean_turnover_rate(
  dat.in.regional.means[,taxon.list],
  bd_component_name = 'repl')

# 3 -- 
mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time)

# 4 -- 
phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd

dat.bd.turnover_rate_repl <- data.frame(
  beta_type = 'turnover_rate_repl',
  gamma_temporal_bd,
  mean_alpha_temporal_bd,
  phi_bd,
  n_locations = length(unique(dat.in.wide.spp$site)),
  n_times = length(unique(dat.in.wide.spp$time)))

################
# -- turnover "rate" rich
################

# 1 -- temporal beta-div at each site
alpha_temporal_bd <- dat.in.wide.spp %>% 
  group_by(site) %>% 
  do(data.frame(bd_time = fn_bd_components_mean_turnover_rate(.[,taxon.list],
                                                              bd_component_name = 'rich')))

# 2 -- temporal beta-div for the metacommunity centroid (regional species pool)
dat.in.regional.means <- dat.in.wide.spp %>% group_by(time) %>%
  select(one_of(taxon.list)) %>%
  summarise_each(funs(mean))
gamma_temporal_bd <- fn_bd_components_mean_turnover_rate(
  dat.in.regional.means[,taxon.list],
  bd_component_name = 'rich')

# 3 -- 
mean_alpha_temporal_bd <- mean(alpha_temporal_bd$bd_time)

# 4 -- 
phi_bd <- gamma_temporal_bd/mean_alpha_temporal_bd

dat.bd.turnover_rate_rich <- data.frame(
  beta_type = 'turnover_rate_rich',
  gamma_temporal_bd,
  mean_alpha_temporal_bd,
  phi_bd,
  n_locations = length(unique(dat.in.wide.spp$site)),
  n_times = length(unique(dat.in.wide.spp$time)))

# combine results
dat.bd <- rbind(dat.bd.turnover_rate,
                dat.bd.turnover_rate_v2,
                dat.bd.turnover_rate_repl,
                dat.bd.turnover_rate_rich,
                dat.bd.cum_path_length,
                dat.bd.cum_path_length_v2,
                dat.bd.cum_path_length_repl,
                dat.bd.cum_path_length_rich,
                dat.bd.total,
                dat.bd.total.v2,
                dat.bd.total.repl,
                dat.bd.total.rich
)