# -- packages
library(dplyr)

# -- simulate data set for testing diversity functions

# -- Install the current dev version of MCSim
# devtools::install_github('sokole/MCSim')

xy.coordinates <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(1, 3, 1, 5, 2))

my.landscape <- MCSim::fn.make.landscape(
  site.coords = xy.coordinates,
  m = c(0.5, 0.5, 0.1, 0.1, 0.01),
  Ef = c(-1, -.25, .1, 1, 2),
  JM = 1000000)

# niche positions, niche breadths, and relative abundances for three species
niche.positions <-  c(-.5, 0, .5)
niche.breadths <- c(.2, .2, 5)
regional.rel.abund <- c(.8, .1, .1)

# -- set random seed
set.seed(1234)

# -- run simulation
sim.result <- MCSim::fn.metaSIM(
  landscape = my.landscape,
  trait.Ef = niche.positions,
  trait.Ef.sd = niche.breadths, 
  gamma.abund = regional.rel.abund,
  W.r = 0,
  nu = 0.001,
  n.timestep = 10, 
  scenario.ID = 'test_data',
  sim.ID = 'test_data',
  output.dir.path = 'simulated_data' 
)

# -- write species counts to csv file
write.csv(sim.result$J.long, 'dat_simulated_species_counts.csv', row.names = FALSE)

d.env <- sim.result$landscape$site.info
d.env.2 <- d.env %>% select(site.ID, area.m2, Ef) %>% 
  rename(env.var = Ef)

# -- make environmental data long format, write to csv
d.env.long <- tidyr::gather(d.env.2, 'env.var','env.value', -site.ID)

write.csv(d.env.long, 'dat_simulated_environment.csv',
          row.names = FALSE)

# -- make spatial data long format, write to csv
d.xy <- data.frame(
  site.ID = d.env.2$site.ID,
  sim.result$landscape$site.coords)
d.spatial.long <- tidyr::gather(d.xy, 'spatial.var','spatial.value', -site.ID)
write.csv(d.spatial.long, 'dat_simulated_site_coords.csv',
  row.names = FALSE)
