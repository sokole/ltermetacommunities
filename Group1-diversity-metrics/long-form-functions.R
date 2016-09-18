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
  df.ems.struc <- (metacom::IdentifyStructure(df.wide.ems))  # prints the structure of the MC
  
}



# # variation partitioning
# fn.varpart.long <- function(
#   site.id.vect,
#   spp.vect,
#   abund.vect,
#   env.vect,
#   spatial.vect,
#   ...){
#   
#   # combine vectors in a data.frame
#   df.long <-  data.frame(
#     site = site.id.vect,
#     spp = spp.vect,
#     abund = abund.vect
#     env = env.vect
#     space = spatial.vect
#   )
#   
#   # average across replicate observations for a site -- if there are multiple observations for a site
#   df.grouped.long <- dplyr::group_by(df.long, site, spp)
#   df.means.long <- dplyr::summarise(df.grouped.long, 
#                                     abund.mean = mean(abund))
#   
#   # change from long format to wide format data.frame
#   df.wide <- tidyr::spread(df.means.long,
#                            key = spp,
#                            value = abund.mean,
#                            fill = 0)[,-1]
#   
#   df.wide <- df.wide[,which(colnames(df.wide) != "")] # removing potential blank cols
#   
#   # calculate diversity metric
#   vegetarian::d(df.wide, wts = rowSums(df.wide), ...)
# }
# 
# comm.date <- filter(mc.species.wide, DATE == date.i)
# common.sites <- dplyr::intersect(dplyr::intersect(comm.date[,1], env.date[,1]), spatial[,1])
# comm.date <- as.matrix(filter(comm.date, SITE_ID %in% common.sites)[,-c(1:2)])
# env.date <- as.matrix(filter(env.date, SITE_ID %in% common.sites)[,-c(1:2)])
# spatial <- as.matrix(filter(spatial, SITE_ID %in% common.sites)[,-1])
# 
# 
# comm.date.hel <- decostand(comm.date, method = "hellinger")
# comm.date.varpart <- vegan::varpart(comm.date.hel, env.date, spatial)
# vp <- vector(length = 4)
# vp <- comm.date.varpart$part$indfract$Adj.R.squared
# vp.a <- vp[1]
# vp.b <- vp[2]
# vp.c <- vp[3]
# vp.d <- vp[4]
