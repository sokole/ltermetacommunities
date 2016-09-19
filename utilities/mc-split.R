fn.mc.split <- function(mc.data = mc.data){
  
  # Create the spatial dataset
  mc.spatial.long <<- filter(mc.data, OBSERVATION_TYPE == "SPATIAL_COORDINATE") %>% 
    select(SITE_ID, VARIABLE_NAME, VALUE)
  
  mc.spatial.wide <<- arrange(
    tidyr::spread(mc.spatial.long, key = VARIABLE_NAME, value = VALUE), SITE_ID)
  
  # Create the environmental dataset
  mc.env.long <<- filter(mc.data, OBSERVATION_TYPE == "ENV_VAR") %>% 
    select(SITE_ID, DATE, VARIABLE_NAME, VALUE)
  
  mc.env.wide <- mc.env.long %>%
  select(-VARIABLE_UNITS) %>%
    tidyr::spread(VARIABLE_NAME,  VALUE)


  
  # Create the species matrix
  mc.species.long <<- filter(na.omit(mc.data), OBSERVATION_TYPE == "TAXON_COUNT") %>%
    select(SITE_ID, DATE, VARIABLE_NAME, VALUE)
  mc.species.wide <<- tidyr::spread(mc.species.long, key = VARIABLE_NAME, value = VALUE, fill = 0)
}