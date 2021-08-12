library(tidyverse)

# testing divpart_renyi
set.seed(123)
dat_example_long <- data.frame(
  my_sites = c(rep('x', 8), rep('y',8)),
  my_times = rep(1:4, each = 2),
  my_spp = rep(c('a','b'), 4),
  my_biomass = (exp(rnorm(16))*10) %>% round(0))

divpart_renyi(
  data_long = dat_example_long,
  site_id_col_name = 'my_sites',
  time_step_col_name = 'my_times',
  taxon_id_col_name = 'my_spp',
  biomass_col_name = 'my_biomass',
  q_value = 0)

divpart_renyi(
  data_long = dat_example_long,
  site_id_col_name = 'my_sites',
  time_step_col_name = 'my_times',
  taxon_id_col_name = 'my_spp',
  biomass_col_name = 'my_biomass',
  q_value = 1)

divpart_renyi(
  data_long = dat_example_long,
  site_id_col_name = 'my_sites',
  time_step_col_name = 'my_times',
  taxon_id_col_name = 'my_spp',
  biomass_col_name = 'my_biomass',
  q_value = 2)

# testing temporal_BD
dat_1site_5times <- data.frame(
  a = c(1, 2, 3, 4, 5),
  b = c(23, 5, 76, 4, 56),
  c = c(0, 0, 12, 41, 1))

dat_1site_5times_with_time_col <- data.frame(
  my_time_step = c(1, 2, 3, 4, 5),
  a = c(1, 2, 3, 4, 5),
  b = c(23, 5, 76, 4, 56),
  c = c(0, 0, 12, 41, 1))

dat_long <- dat_1site_5times_with_time_col %>%
  pivot_longer(cols = c("a","b","c"),
               names_to = "my_species",
               values_to = "my_biomass")

# examples
# wide data, only species cols, rows ordered by time
temporal_BD(data_in = dat_1site_5times)

# wide data, indicate species and time_step columns
temporal_BD(data_in = dat_1site_5times_with_time_col,
            taxon_list = c('a', 'b', 'c'), time_step_col_name = 'my_time_step')

# long data
temporal_BD(data_in = dat_long,
            time_step_col_name = 'my_time_step',
            taxon_id_col_name = 'my_species',
            biomass_col_name = 'my_biomass')

# testing local_variability
dat_example_long <- data.frame(
  my_sites = c(rep('x', 8), rep('y',8)),
  my_times = rep(1:4, each = 2),
  my_spp = rep(c('a','b'), 4),
  my_biomass = (exp(rnorm(16))*10) %>% round(0))

# Variability in community composition using the
# modified Hellinger metric
local_variability(
  data_long = dat_example_long,
  time_step_col_name = 'my_times',
  site_id_col_name = 'my_sites',
  taxon_id_col_name = 'my_spp',
  biomass_col_name = 'my_biomass',
  standardization_method = 'h',
  variability_type = 'comp')

# Variability in aggregate biomass folloing
# Wang and Loreau (2014)
local_variability(
  data_long = dat_example_long,
  time_step_col_name = 'my_times',
  site_id_col_name = 'my_sites',
  taxon_id_col_name = 'my_spp',
  biomass_col_name = 'my_biomass',
  variability_type = 'agg')

# examples with wide data
dat_example_wide <- dat_example_long %>% spread(my_spp, my_biomass)

divpart_renyi(
  data_wide = dat_example_wide,
  site_id_col_name = 'my_sites',
  time_step_col_name = 'my_times',
  taxon_list = c('a','b'),
  biomass_col_name = 'my_biomass',
  q_value = 0)


############################################################

dat_example_long <- data.frame(
  my_sites = c(rep('x', 8), rep('y',8)),
  my_times = rep(1:4, each = 2),
  my_spp = rep(c('a','b'), 4),
  my_biomass = (exp(rnorm(16))*10) %>% round(0))


# temporal beta diversity for site x -- not standardized
temporal_BD(data_in = dat_example_long %>%
              dplyr::filter(my_sites == "x"),
            time_step_col_name = "my_times",
            taxon_id_col_name = "my_spp",
            biomass_col_name = "my_biomass")


# local variability
local_variability(data_long = dat_example_long,
                  time_step_col_name = 'my_times',
                  site_id_col_name = 'my_sites',
                  taxon_id_col_name = 'my_spp',
                  biomass_col_name = 'my_biomass',
                  variability_type = 'comp')




# Variability in community composition using the
# modified Hellinger metric
metacommunity_variability(
  data_long = dat_example_long,
  time_step_col_name = 'my_times',
  site_id_col_name = 'my_sites',
  taxon_id_col_name = 'my_spp',
  biomass_col_name = 'my_biomass',
  variability_type = 'comp')

metacommunity_variability(
  data_long = dat_example_long,
  time_step_col_name = 'my_times',
  site_id_col_name = 'my_sites',
  taxon_id_col_name = 'my_spp',
  biomass_col_name = 'my_biomass',
  variability_type = 'agg')

spp_list <- dat_example_long$my_spp %>% unique()
n_sites <- dat_example_long$my_sites %>% unique() %>% length()
n_years <- dat_example_long$my_times %>% unique() %>% length()



dat_example_wide <- dat_example_long %>%
  pivot_wider(id_cols = c(my_sites,my_times),
              names_from = my_spp,
              values_from = my_biomass,
              values_fill = 0)

# temporal beta diversity for site x -- not standardized

# specify columns
temporal_BD(data_in = dat_example_wide %>%
              dplyr::filter(my_sites == "x"),
            time_step_col_name = "my_times",
            taxon_list = spp_list)

# assume all columns are species, all rows are temporal reps
temporal_BD(data_in = dat_example_wide %>%
              dplyr::filter(my_sites == "x") %>%
              select(c(a,b)))

# local variability
local_variability(data_wide = dat_example_wide,
                  time_step_col_name = 'my_times',
                  site_id_col_name = 'my_sites',
                  taxon_list = c("a","b"),
                  variability_type = 'comp')


# using Thomas' fxn
space_stab(Y = dat_example_wide[,spp_list],
           s = n_sites,
           t = n_years)

# explicit species list
metacommunity_variability(data_wide = dat_example_wide,
                          time_step_col_name = 'my_times',
                          site_id_col_name = 'my_sites',
                          taxon_list = spp_list,
                          variability_type = 'agg')

metacommunity_variability(data_wide = dat_example_wide,
                          time_step_col_name = 'my_times',
                          site_id_col_name = 'my_sites',
                          taxon_list = spp_list,
                          variability_type = 'comp')

# implied species list
metacommunity_variability(data_wide = dat_example_wide,
                          time_step_col_name = 'my_times',
                          site_id_col_name = 'my_sites',
                          variability_type = 'agg')

metacommunity_variability(data_wide = dat_example_wide,
                          time_step_col_name = 'my_times',
                          site_id_col_name = 'my_sites',
                          variability_type = 'comp')


#####################################################

dat_wide <- data.frame(
  my_sites = rep(1:2, each = 4),
  my_times = rep(1:4, times = 2),
  a = c(1, 4, 2, 0, 2, 3, 0, 9),
  b = c(1, 3, 3, 4, 0, 0, 0, 0))

spp_list <- c("a","b")
n_sites <- dat_wide$my_sites %>% unique() %>% length()
n_years <- dat_wide$my_times %>% unique() %>% length()

# temporal beta diversity for site x -- not standardized

# specify columns
temporal_BD(data_in = dat_wide %>%
              dplyr::filter(my_sites == 1),
            time_step_col_name = "my_times",
            taxon_list = spp_list)

# assume all columns are species, all rows are temporal reps
temporal_BD(data_in = dat_wide %>%
              dplyr::filter(my_sites == 1) %>%
              select(c(a,b)))

# local variability
local_variability(data_wide = dat_example_wide,
                  time_step_col_name = 'my_times',
                  site_id_col_name = 'my_sites',
                  taxon_list = c("a","b"),
                  variability_type = 'comp')



# Thomas' fxn
space_stab(Y = dat_wide[,spp_list],
           s = n_sites,
           t = n_years)

# explicit species list
metacommunity_variability(data_wide = dat_wide,
                          time_step_col_name = 'my_times',
                          site_id_col_name = 'my_sites',
                          taxon_list = spp_list,
                          variability_type = 'agg')

metacommunity_variability(data_wide = dat_wide,
                          time_step_col_name = 'my_times',
                          site_id_col_name = 'my_sites',
                          taxon_list = spp_list,
                          variability_type = 'comp')

#implied species list
metacommunity_variability(data_wide = dat_wide,
                          time_step_col_name = 'my_times',
                          site_id_col_name = 'my_sites',
                          variability_type = 'agg')

metacommunity_variability(data_wide = dat_wide,
                          time_step_col_name = 'my_times',
                          site_id_col_name = 'my_sites',
                          variability_type = 'comp')


dat_long <- dat_wide %>%
  pivot_longer(cols = c(a,b),
               names_to = "my_spp")


# temporal beta diversity for site x -- not standardized
temporal_BD(data_in = dat_long %>%
              dplyr::filter(my_sites == 1),
            time_step_col_name = "my_times",
            taxon_id_col_name = "my_spp",
            biomass_col_name = "value")


# local variability
local_variability(data_long = dat_long,
                  time_step_col_name = 'my_times',
                  site_id_col_name = 'my_sites',
                  taxon_id_col_name = 'my_spp',
                  biomass_col_name = 'value',
                  variability_type = 'comp')


# metacomm variability
metacommunity_variability(data_long = dat_long,
                          time_step_col_name = 'my_times',
                          site_id_col_name = 'my_sites',
                          taxon_id_col_name = 'my_spp',
                          biomass_col_name = 'value',
                          variability_type = 'agg')

metacommunity_variability(data_long = dat_long,
                          time_step_col_name = 'my_times',
                          site_id_col_name = 'my_sites',
                          taxon_id_col_name = 'my_spp',
                          biomass_col_name = 'value',
                          variability_type = 'comp')
