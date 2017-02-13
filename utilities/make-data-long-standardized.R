###############################################
# -- format data from wide to long, use standardized column headers
# 
# obs_type_label -- should be "SPATIAL_COORDINATE", "ENV_VAR", or "TAXON_COUNT"
# site_id_col_name -- indicates site ID column in the data set
# site_date_col_name -- indicates the column with time data. If NA, then one time per site with a date = NA
# variable_names -- column names for variables to expand to long format
# value units -- list of units for each variable name, in same order
#     
###############################################
make_data_long = function(
  data_in,
  obs_type_label,
  site_id_col_name = NA,
  site_date_col_name = NA,
  variable_names,
  value_units
){
  ## use rownames if no site_id_col_name is given
  if(is.na(site_id_col_name)){
    data_in$site_id <- row.names(data_in)
    site_id_col_name <- 'site_id'
  }
  ## make a key for units
  d.unit.key <- data.frame(variable_names,
                           value_units)
  
  ## dealing with site locations -- no date
  if(is.na(site_date_col_name)){
    site_date_col_name <- 'NO-DATE'
    data_in[,site_date_col_name] <- NA}
  
  ## make data long format
  data.long <- reshape2::melt(
    data_in,
    id.vars = c(site_id_col_name,
                site_date_col_name),
    measure.vars = variable_names
  )
  
  ## join units
  data.long <- dplyr::left_join(
    data.long,
    d.unit.key,
    by = c('variable' = 'variable_names')
  )
  
  ## return a long format data frame   
  return(
    data.frame(
      OBSERVATION_TYPE = obs_type_label,
      SITE_ID = data.long[,site_id_col_name],
      DATE = data.long[,site_date_col_name],
      VARIABLE_NAME = data.long$variable,
      VARIABLE_UNITS = data.long$value_units,
      VALUE = data.long$value
    )
  )
}  

#####################################################
# Example with NWT data set
#####################################################
# 
# d.plants <- read.csv('saddle_plantcomp_LMH.csv', row.names = 1)
# d.plants.sub <- d.plants[,c('USDA_code','plot','year','abund')]
# 
# ### propagate species
# d.plants.sub.wide <- reshape2::dcast(
#   d.plants.sub,
#   plot+year ~ USDA_code,
#   value.var = 'abund',
#   fill = 0
# )
# spp.list <- names(d.plants.sub.wide)[-c(1:2)]
# 
# 
# d.snow <- read.csv('NWT_SaddleData_snowdepth.csv', row.names = 1)
# 
# d.space <- read.csv('NWT_coordinates.csv')
# d.space$plot <- d.space$grid_point
# 
# d.merged <- dplyr::left_join(
#   d.plants,
#   d.snow,
#   by = c('plot', 'year')
# )
# 
# d.spatial <- make_data_long(
#   data_in = d.space,
#   obs_type_label = 'SPATIAL_COORDINATE',
#   site_id_col_name = 'plot',
#   site_date_col_name = NA,
#   variable_names = c('x_coord_m', 'y_coord_m'),
#   value_units = c('m','m')
# )
# 
# d.env <- make_data_long(
#   data_in = d.merged,
#   obs_type_label = 'ENV_VAR',
#   site_id_col_name = 'plot',
#   site_date_col_name = 'year',
#   variable_names = 'max_snow',
#   value_units = c('in')
# )
# 
# d.comm <- make_data_long(
#   data_in = d.plants.sub.wide,
#   obs_type_label = 'TAXON_COUNT',
#   site_id_col_name = 'plot',
#   site_date_col_name = 'year',
#   variable_names = spp.list,
#   value_units = c('count')
# )
# 
# d.long.long <- rbind(d.comm, d.spatial, d.env)

# write.csv(d.long.long, file = 'NWT-saddle-plants-and-snow-LONG.csv', row.names = FALSE)

