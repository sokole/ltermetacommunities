# diversity partitioning

fn.divpart.long <- function(
  site.id.vect,
  spp.vect,
  abund.vect,
  ...){
  
  # combine vectors in a data.frame
  df.long <-  data.frame(
    site = site.id.vect,
    spp = spp.vect,
    abund = abund.vect
  )
  
  # average across replicate observations for a site -- if there are multiple observations for a site
  df.grouped.long <- dplyr::group_by(df.long, site, spp)
  df.means.long <- dplyr::summarise(df.grouped.long, 
                                    abund.mean = mean(abund))
  
  # change from long format to wide format data.frame
  df.wide <- tidyr::spread(df.means.long,
                           key = spp,
                           value = abund.mean,
                           fill = 0)[,-1]
  
  df.wide <- df.wide[,which(colnames(df.wide) != "")] # removing potential blank cols
  
  # calculate diversity metric
  vegetarian::d(df.wide, wts = rowSums(df.wide), ...)
}




# ems
fn.ems.long <- function(
  site.id.vect,
  spp.vect,
  abund.vect,
  ...){
  
  # combine vectors in a data.frame
  df.long <-  data.frame(
    site = site.id.vect,
    spp = spp.vect,
    abund = abund.vect
  )
  
  # change from long format to wide format data.frame
  df.wide <- tidyr::spread(df.long,
                           key = spp,
                           value = abund,
                           fill = 0)
  
  df.wide <- select(df.wide[,which(colnames(df.wide) != "")], -site) # removing potential blank cols
  
  # calculate ems
  
  df.wide.pa <- vegan::decostand(df.wide, method = "pa")
  df.wide.pa <- df.wide.pa[,which(colSums(df.wide.pa) > 0)] # remove empty cols and rows
  df.wide.pa <- df.wide.pa[which(rowSums(df.wide.pa) > 0),]
  df.wide.ems <- metacom::Metacommunity(
    df.wide.pa, ...)
  metacom::IdentifyStructure(df.wide.ems)  # prints the structure of the MC
  
}



# variation partitioning
fn.varpart.long <- function(
  OBSERVATION_TYPE,
  SITE_ID,
  VARIABLE_NAME,
  VALUE,
  ...){
  
  mc <- tbl_df(data_frame(OBSERVATION_TYPE, SITE_ID, VARIABLE_NAME, VALUE))
  
  # combine vectors in a data.frame
  species.df.long <- select(filter(mc, OBSERVATION_TYPE == "TAXON_COUNT"),
                            site = SITE_ID, spp = VARIABLE_NAME, abund = VALUE)
  
  env.df.long <- select(filter(mc, OBSERVATION_TYPE == "ENV_VAR"),
                        site = SITE_ID, env.var = VARIABLE_NAME, env.val = VALUE)
  
  space.df.long <- select(filter(mc, OBSERVATION_TYPE == "SPATIAL_COORDINATE"),
                          site = SITE_ID, space.var = VARIABLE_NAME, space.val = VALUE)
  

  # change from long format to wide format data.frame
  species.df.wide <- tidyr::spread(na.omit(species.df.long),
                                   key = spp,
                                   value = abund,
                                   fill = 0)
  
  env.df.wide <- tidyr::spread(na.omit(env.df.long),
                               key = env.var,
                               value = env.val,
                               fill = 0)
  
  space.df.wide <- tidyr::spread(na.omit(space.df.long),
                                 key = space.var,
                                 value = space.val,
                                 fill = 0)
  

  total.df.wide <- left_join(left_join(species.df.wide, env.df.wide), space.df.wide)

  
  species.df <- (total.df.wide[,colnames(species.df.wide)][,-1])
  env.df <- (total.df.wide[,colnames(env.df.wide)][,-1])
  space.df <- (total.df.wide[,colnames(space.df.wide)][,-1])
  
  species.hel <- decostand(species.df, method = "hellinger")
  vegan::varpart(species.hel, env.df, space.df)

}