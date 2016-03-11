# # required packages
# dplyr
# tidyr
# vegan
# vegetarian

library(dplyr)

# -- read data
d.env <- read.csv('DATA_IN/dat_simulated_environment.csv')
d.comm.long <- read.csv('DATA_IN/dat_simulated_species_counts.csv')
d.xy <- read.csv('DATA_IN/dat_simulated_site_coords.csv')

# ---------------------------------------------------------------
# -- FUNCTIONS that work on a single long comm matrix

# ---------------------------------------------------------------

# -- make vegetarian::d compatible with long data, send it two vectors
fn.divpart.long <- function(
  spp.vect,
  abund.vect,
  ...){
  df.long <- data.frame(spp.vect, abund.vect)
  df.wide <- tidyr::spread(df.long, spp.vect, abund.vect,
                           fill = 0)
  vegetarian::d(df.wide, wts = rowSums(df.wide), ...)
}

# ---------------------------------------------------------------

# -- group by time

dat_diversities_by_timestep <- d.comm.long %>% group_by(timestep) %>% 
  summarise(
    alpha_q1 = fn.divpart.long(spp, count, lev = 'alpha', q = 1),
    beta_q1 = fn.divpart.long(spp, count, lev = 'beta', q = 1),
    gamma_q1 = fn.divpart.long(spp, count, lev = 'gamma', q = 1),
    alpha_q2 = fn.divpart.long(spp, count, lev = 'alpha', q = 2),
    beta_q2 = fn.divpart.long(spp, count, lev = 'beta', q = 2),
    gamma_q2 = fn.divpart.long(spp, count, lev = 'gamma', q = 2)
  )

write.csv(dat_diversities_by_timestep, 'data_out_diversities_by_timestep.csv', row.names = FALSE)

