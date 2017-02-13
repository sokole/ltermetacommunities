##################################################
# -- load function from untilities directory
##################################################
# devtools::source_url('https://raw.githubusercontent.com/sokole/ltermetacommunities/master/utilities/make-data-long-standardized.R')

##################################################
# -- packages
##################################################
library(dplyr)

##################################################
# -- simulate data set for testing diversity functions
##################################################

# -- Install the current dev version of MCSim
# -- update version of MCSim if needed
devtools::install_github('sokole/MCSim')

# -- set random seed for reproducible results
set.seed(1234)

# -- number of patches in metacommunity
n.patches <- 10

# -- number of timesteps
n.time.steps <- 10

# -- coordinates of patches
xy.coordinates <- data.frame(
  x = sample.int(100, n.patches),
  y = sample.int(100, n.patches))

# -- make a landscape object for the simulation
my.landscape <- MCSim::fn.make.landscape(
  site.coords = xy.coordinates,
  m = sample(c(0.5, 0.1, 0.01), n.patches, replace = T),
  Ef = sample(c(-1, -.25, .1, 1, 2), n.patches, replace = T),
  JM = 1000000)

# niche positions, niche breadths, and relative abundances for three species
niche.positions <-  c(-.5, 0, .5)
niche.breadths <- c(.2, .2, 5)
regional.rel.abund <- c(.8, .1, .1)

# -- run simulation
sim.result <- MCSim::fn.metaSIM(
  landscape = my.landscape,
  trait.Ef = niche.positions,
  trait.Ef.sd = niche.breadths, 
  gamma.abund = regional.rel.abund,
  W.r = 0,
  nu = 0.001,
  n.timestep = n.time.steps, 
  scenario.ID = 'test_data',
  sim.ID = 'test_data',
  output.dir.path = 'simulated_data' 
)

# -- extract taxon data
d_TAXON_COUNT <- data.frame(
  OBSERVATION_TYPE = 'TAXON_COUNT',
  SITE_ID = sim.result$J.long$site,
  DATE = sim.result$J.long$timestep,
  VARIABLE_NAME = sim.result$J.long$spp,
  VARIABLE_UNITS = 'COUNT',
  VALUE = sim.result$J.long$count
)

# -- extract spatial data
d_SPATIAL_COORDINATE <- make_data_long(
  obs_type_label = 'SPATIAL_COORDINATE',
  data = sim.result$landscape$site.coords,
  variable_names = c('x','y'),
  value_units = c('m')
)

# -- extract "environmental" data, make observations for each time period, eventhough env doesn't vary over time
d.env.temp <- d_TAXON_COUNT %>% select(SITE_ID, DATE) %>%
  left_join(sim.result$landscape$site.info, 
            by = c('SITE_ID' = 'site.ID')) %>% 
  mutate(env_fixed = Ef,
         env_rnd = runif(length(DATE), min = -1, max = 1))

d_ENV_VAR <- make_data_long(
  obs_type_label = 'ENV_VAR',
  site_date_col_name = 'DATE',
  data = d.env.temp,
  variable_names = c('env_fixed','env_rnd'),
  value_units = c('scaled_env_state_var')
) 

d_complete_data_set <- rbind(
  d_SPATIAL_COORDINATE,
  d_ENV_VAR,
  d_TAXON_COUNT
)

# -- write species counts to csv file
write.csv(d_complete_data_set, 'dat_simulated_metacommunity_timeseries.csv', row.names = FALSE)
