# library(popler)
# library(rvest)
library(dplyr)
library(tidyr)
options(stringsAsFactors = F)

#Check to make sure working directory is set to the ltermetacommunities github
if(basename(getwd())!="ltermetacommunities"){cat("Plz change your working directory. It should be 'ltermetacommunities'")}

# Check for and install required packages
for (package in c('googledrive','dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom', 'ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# get mollusc data links from popler
mollusc_links <- pplr_browse(proj_metadata_key == 298,
                             full_tbl=T)$metalink %>%
                    strsplit('; ') %>%
                    unlist( recursive = F )

# 
# # links to dois
# get_text <- function(x, node_str){
#   
#   read_html(x) %>%
#     html_nodes(node_str) %>%       # find all links
#     html_text()
#   
# }
# 
# # open the URLs
# gce_dois <- mollusc_links %>%
#               # remove white spaces
#               sapply(trimws) %>% 
#               # get DOIS
#               sapply( get_text, 'tr:nth-child(5) a' ) %>% 
#               # paste actual DOIs
#               sapply(function(x) paste0( 'http://dx.doi.org/',x) ) %>% 
#               # remove names
#               setNames( NULL )
# 
# # get titles
# gce_titl <- gce_dois %>%
#               # get DOIS
#               sapply( get_text, '.table-row:nth-child(1) li' )  %>% 
#               # extract_season 
#               sapply( function(x)
#                         regmatches(x, 
#                                    gregexpr("[A-z]{4,6} [[:digit:]]{4}", 
#                                    x) )
#               ) %>% 
#               # remove names and make this a vector
#               setNames( NULL ) %>% 
#               unlist
# 
# # only keep links with "Fall" data!
# keep_t <- grep('Fall', gce_titl)
# 
# # copy and paste links BY HAND
# sapply(gce_dois[keep_t], browseURL)
# 
df       <- list()
# df_meta  <- data.frame( doi   = gce_dois[keep_t],
#     
#                                             title = gce_titl[keep_t] )


# write ------------------------------------------------------------------------------------------------------
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/459/4/ab7d98b17f4eed0ca9cb7387230a2220" 
infile1 <- sub("^https","http",infile1) 
df[[1]] <-read.csv(infile1,header=F 
          ,skip=5
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Year",     
                    "Month",     
                    "Day",     
                    "Site",     
                    "Zone",     
                    "Plot",     
                    "Location",     
                    "Location_Notes",     
                    "Longitude",     
                    "Latitude",     
                    "Species",     
                    "Mollusc_Count",     
                    "Quadrat_Area",     
                    "Mollusc_Density"    ), check.names=TRUE)

# 2
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/434/11/842b589768ef59db880f50b258adaf46" 
infile1 <- sub("^https","http",infile1) 
df[[2]] <-read.csv(infile1,header=F 
                    ,skip=5
                    ,sep=","  
                    ,quot='"' 
                    , col.names=c(
                    "Year",     
                    "Month",     
                    "Day",     
                    "Site",     
                    "Zone",     
                    "Plot",     
                    "Location",     
                    "Location_Notes",     
                    "Longitude",     
                    "Latitude",     
                    "Species",     
                    "Mollusc_Count",     
                    "Quadrat_Area",     
                    "Mollusc_Density"    ), check.names=TRUE)

# 3
infile1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/429/6/60ca9f877a5704f86495b74535d0e362" 
infile1 <- sub("^https","http",infile1) 
df[[3]] <-read.csv(infile1,header=F 
          ,skip=5
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Year",     
                    "Month",     
                    "Day",     
                    "Site",     
                    "Zone",     
                    "Plot",     
                    "Location",     
                    "Flag_Location",     
                    "Location_Notes",     
                    "Longitude",     
                    "Flag_Longitude",     
                    "Latitude",     
                    "Flag_Latitude",     
                    "Species",     
                    "Mollusc_Count",     
                    "Flag_Mollusc_Count",     
                    "Quadrat_Area",     
                    "Flag_Quadrat_Area",     
                    "Mollusc_Density",     
                    "Flag_Mollusc_Density"    ), check.names=TRUE)

# 4
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/314/20/042648448c32681412161c44e0d7e293" 
infile1 <- sub("^https","http",infile1) 
df[[4]] <-read.csv(infile1,header=F 
          ,skip=5
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Year",     
                    "Month",     
                    "Day",     
                    "Site",     
                    "Zone",     
                    "Plot",     
                    "Location",     
                    "Flag_Location",     
                    "Location_Notes",     
                    "Longitude",     
                    "Flag_Longitude",     
                    "Latitude",     
                    "Flag_Latitude",     
                    "Species",     
                    "Mollusc_Count",     
                    "Flag_Mollusc_Count",     
                    "Quadrat_Area",     
                    "Flag_Quadrat_Area",     
                    "Mollusc_Density",     
                    "Flag_Mollusc_Density"    ), check.names=TRUE)

# 5
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/329/20/c5c887097660d8b717020549a9d3416e" 
infile1 <- sub("^https","http",infile1) 
df[[5]] <-read.csv(infile1,header=F 
          ,skip=5
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Year",     
                    "Month",     
                    "Day",     
                    "Site",     
                    "Zone",     
                    "Plot",     
                    "Location",     
                    "Flag_Location",     
                    "Location_Notes",     
                    "Longitude",     
                    "Flag_Longitude",     
                    "Latitude",     
                    "Flag_Latitude",     
                    "Species",     
                    "Mollusc_Count",     
                    "Flag_Mollusc_Count",     
                    "Quadrat_Area",     
                    "Flag_Quadrat_Area",     
                    "Mollusc_Density",     
                    "Flag_Mollusc_Density",     
                    "Notes"    ), check.names=TRUE)

# 6
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/306/30/a83a44d1425c0896ae00812a08f49e23" 
infile1 <- sub("^https","http",infile1) 
df[[6]] <-read.csv(infile1,header=F 
          ,skip=5
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Year",     
                    "Month",     
                    "Day",     
                    "Site",     
                    "Zone",     
                    "Plot",     
                    "Location",     
                    "Flag_Location",     
                    "Location_Notes",     
                    "Longitude",     
                    "Flag_Longitude",     
                    "Latitude",     
                    "Flag_Latitude",     
                    "Species",     
                    "Mollusc_Count",     
                    "Flag_Mollusc_Count",     
                    "Quadrat_Area",     
                    "Flag_Quadrat_Area",     
                    "Mollusc_Density",     
                    "Flag_Mollusc_Density"    ), check.names=TRUE)

# 7
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/297/20/9e874ef5b244067b82a6c4678f45439f" 
infile1 <- sub("^https","http",infile1) 
df[[7]] <-read.csv(infile1,header=F 
          ,skip=5
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Year",     
                    "Month",     
                    "Day",     
                    "Site",     
                    "Zone",     
                    "Plot",     
                    "Location",     
                    "Flag_Location",     
                    "Location_Notes",     
                    "Longitude",     
                    "Flag_Longitude",     
                    "Latitude",     
                    "Flag_Latitude",     
                    "Species",     
                    "Mollusc_Count",     
                    "Flag_Mollusc_Count",     
                    "Quadrat_Area",     
                    "Flag_Quadrat_Area",     
                    "Mollusc_Density",     
                    "Flag_Mollusc_Density"    ), check.names=TRUE)

# 8 
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/292/20/a6b04f08212d3a065e36b96a119ae05c" 
infile1 <- sub("^https","http",infile1) 
df[[8]] <-read.csv(infile1,header=F 
          ,skip=5
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Year",     
                    "Month",     
                    "Day",     
                    "Site",     
                    "Zone",     
                    "Plot",     
                    "Location",     
                    "Flag_Location",     
                    "Location_Notes",     
                    "Longitude",     
                    "Flag_Longitude",     
                    "Latitude",     
                    "Flag_Latitude",     
                    "Species",     
                    "Mollusc_Count",     
                    "Flag_Mollusc_Count",     
                    "Quadrat_Area",     
                    "Flag_Quadrat_Area",     
                    "Mollusc_Density",     
                    "Flag_Mollusc_Density"    ), check.names=TRUE)

# 9 
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/236/20/294804188e3d3e5650f0816d90f9ed9d" 
infile1 <- sub("^https","http",infile1) 
df[[9]] <-read.csv(infile1,header=F 
          ,skip=5
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Year",     
                    "Month",     
                    "Day",     
                    "Site",     
                    "Zone",     
                    "Plot",     
                    "Location",     
                    "Flag_Location",     
                    "Location_Notes",     
                    "Longitude",     
                    "Flag_Longitude",     
                    "Latitude",     
                    "Flag_Latitude",     
                    "Species",     
                    "Mollusc_Count",     
                    "Flag_Mollusc_Count",     
                    "Quadrat_Area",     
                    "Flag_Quadrat_Area",     
                    "Mollusc_Density",     
                    "Flag_Mollusc_Density"    ), check.names=TRUE)

# 10
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/232/20/5c9c702d8e5d89b4b35509303a627b8b" 
infile1  <- sub("^https","http",infile1) 
df[[10]] <-read.csv(infile1,header=F 
          ,skip=5
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Year",     
                    "Month",     
                    "Day",     
                    "Site",     
                    "Zone",     
                    "Plot",     
                    "Location",     
                    "Flag_Location",     
                    "Location_Notes",     
                    "Longitude",     
                    "Flag_Longitude",     
                    "Latitude",     
                    "Flag_Latitude",     
                    "Species",     
                    "Mollusc_Count",     
                    "Flag_Mollusc_Count",     
                    "Quadrat_Area",     
                    "Flag_Quadrat_Area",     
                    "Mollusc_Density",     
                    "Flag_Mollusc_Density"    ), check.names=TRUE)

# 11
infile1   <- "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/109/21/7319886f0b5feabb0007c2c8bfe9092d" 
infile1   <- sub("^https","http",infile1) 
df[[11]] <-read.csv(infile1,header=F 
          ,skip=5
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Year",     
                    "Month",     
                    "Day",     
                    "Site",     
                    "Zone",     
                    "Plot",     
                    "Location",     
                    "Flag_Location",     
                    "Location_Notes",     
                    "Longitude",     
                    "Flag_Longitude",     
                    "Latitude",     
                    "Flag_Latitude",     
                    "Species",     
                    "Mollusc_Count",     
                    "Flag_Mollusc_Count",     
                    "Quadrat_Area",     
                    "Flag_Quadrat_Area",     
                    "Mollusc_Density",     
                    "Flag_Mollusc_Density"    ), check.names=TRUE)

# 12
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/30/21/2b860791009ec70dfcfb73419c23adc5" 
infile1 <- sub("^https","http",infile1) 
df[[12]] <-read.csv(infile1,header=F 
          ,skip=5
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Year",     
                    "Month",     
                    "Day",     
                    "Site",     
                    "Zone",     
                    "Plot",     
                    "Location",     
                    "Flag_Location",     
                    "Location_Notes",     
                    "Longitude",     
                    "Flag_Longitude",     
                    "Latitude",     
                    "Flag_Latitude",     
                    "Species",     
                    "Mollusc_Count",     
                    "Flag_Mollusc_Count",     
                    "Quadrat_Area",     
                    "Flag_Quadrat_Area",     
                    "Mollusc_Density",     
                    "Flag_Mollusc_Density"    ), check.names=TRUE)

# 13
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/427/6/38673101281fdedda4b8f3b85c0c2cb2" 
infile1 <- sub("^https","http",infile1) 
df[[13]] <-read.csv(infile1,header=F 
          ,skip=5
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Year",     
                    "Month",     
                    "Day",     
                    "Site",     
                    "Zone",     
                    "Plot",     
                    "Location",     
                    "Flag_Location",     
                    "Location_Notes",     
                    "Longitude",     
                    "Flag_Longitude",     
                    "Latitude",     
                    "Flag_Latitude",     
                    "Species",     
                    "Mollusc_Count",     
                    "Flag_Mollusc_Count",     
                    "Quadrat_Area",     
                    "Flag_Quadrat_Area",     
                    "Mollusc_Density",     
                    "Flag_Mollusc_Density"    ), check.names=TRUE)

# 14
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/299/21/44d8b1444a1b0ddd4856abe3f3978df6" 
infile1  <- sub("^https","http",infile1) 
df[[14]] <-read.csv(infile1,header=F 
          ,skip=5
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Year",     
                    "Month",     
                    "Day",     
                    "Site",     
                    "Zone",     
                    "Plot",     
                    "Location",     
                    "Flag_Location",     
                    "Location_Notes",     
                    "Longitude",     
                    "Flag_Longitude",     
                    "Latitude",     
                    "Flag_Latitude",     
                    "Species",     
                    "Mollusc_Count",     
                    "Flag_Mollusc_Count",     
                    "Quadrat_Area",     
                    "Flag_Quadrat_Area",     
                    "Mollusc_Density",     
                    "Flag_Mollusc_Density"    ), check.names=TRUE)


# attempt to put it all together -----------------------------------------------------------------------

# common columns
all_cols   <- lapply(df, names)

# all cases of column names
all_cases  <- all_cols %>% unlist %>% unique

# what is present in ALL cases?
cases <- sapply(all_cases, function(x)
                sapply(all_cols, function(y) x %in% y ) %>%  all ) %>% 
                as.data.frame %>% 
                tibble::add_column(row.names(.),.before=1) %>% 
                setNames( c('col_name','logical') )

# put it all together
all_df <- dplyr::bind_rows( df ) %>% 
            dplyr::select( subset(cases,logical)$col_name ) %>% 
            dplyr::select( -Location_Notes ) %>% 
            subset( !is.na(Mollusc_Density) ) %>% 
            # average by Plot
            group_by(Year, Species, Site, Zone) %>% 
            # remove NAs, as these do not seem to be zeros
            summarise( Mollusc_Density = mean(Mollusc_Density,na.rm=T) ) %>% 
            ungroup %>% 
            # ltermetacomm format!
            dplyr::rename( VALUE            = Mollusc_Density,
                           VARIABLE_NAME    = Species,
                           DATE             = Year ) %>% 
                   mutate( SITE_ID          = paste(Site, Zone,sep='_'), # Plot, 
                           OBSERVATION_TYPE = "TAXON_COUNT",
                           VARIABLE_UNITS   = "DENSITY",
                           VARIABLE_NAME    = as.character(VARIABLE_NAME) ) %>% 
           dplyr::select( -Site, -Zone ) %>%
           dplyr::arrange( DATE, SITE_ID, VARIABLE_NAME ) %>% 
           # remove species "Slug" (it doesn't really mean anything!)
           subset( !(VARIABLE_NAME %in% 'Slug') ) %>% 
           # introduce zeros
           spread(key = VARIABLE_NAME, value = VALUE, fill = 0) %>% 
           gather(key = VARIABLE_NAME, value = VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS) 
           
# only keep sites that are present in every single year
keep_site <- all_df %>% 
                select(DATE, SITE_ID) %>% 
                unique %>% 
                count(SITE_ID) %>% 
                subset( n == 14 ) %>% 
                .$SITE_ID

# records to keep 
all_df_keep <- all_df %>% 
                  # sites that are present in every single year
                  subset( SITE_ID %in% keep_site ) %>% 
                  # Remove un-identified hybrid
                  subset( !(VARIABLE_NAME %in% 'Hydrobiidae' ) )


# Spatial coordinates --------------------------------------------------

# Calculate Lat/Lons
crd_df  <- dplyr::bind_rows( df ) %>% 
            dplyr::select( subset(cases,logical)$col_name ) %>% 
            dplyr::select( -Location_Notes ) %>% 
            subset( !is.na(Mollusc_Density) ) %>% 
            mutate( SITE_ID = paste(Site, Zone, sep='_') ) %>% 
            # sites that are present in every single year
            subset( SITE_ID %in% keep_site ) %>% 
            # average lat/lon by Plot
            group_by( SITE_ID ) %>% 
            summarise( lat = mean(Latitude,  na.rm=T),
                       lon = mean(Longitude, na.rm=T) )



plot_lon    <- crd_df %>% 
                  mutate( OBSERVATION_TYPE = "ENV_VAR", 
                          DATE = NA, # NA because non-temporal
                          VARIABLE_NAME = "longitude", 
                          VARIABLE_UNITS = "dec.degrees") %>% 
                  rename( VALUE = lon ) 

plot_lat    <- crd_df %>% 
                  mutate( OBSERVATION_TYPE = "ENV_VAR", 
                          DATE = NA, # NA because non-temporal
                          VARIABLE_NAME = "latitude", 
                          VARIABLE_UNITS = "dec.degrees") %>% 
                  rename( VALUE = lat ) 
                  

# Stack longitude and latitude together
plot_coord  <- bind_rows(plot_lon,plot_lat) %>% 
                  dplyr::select(-lon,-lat)


# put it all out ---------------------------------------------


out <- bind_rows(all_df_keep, plot_coord)

write.csv(out, 
          'C:/L3-gce-mollusc-compagnoni.csv',
          row.names=F)

# # pasta links copied by hand
# link_v <- c("https://pasta.lternet.edu/package/data/eml/knb-lter-gce/459/4/ab7d98b17f4eed0ca9cb7387230a2220",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/434/11/842b589768ef59db880f50b258adaf46",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/429/6/60ca9f877a5704f86495b74535d0e362",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/329/20/c5c887097660d8b717020549a9d3416e",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/314/20/042648448c32681412161c44e0d7e293",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/306/30/a83a44d1425c0896ae00812a08f49e23",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/297/20/9e874ef5b244067b82a6c4678f45439f",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/294/20/213fd49941cfea9334e6e5d2e31c42f7",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/292/20/a6b04f08212d3a065e36b96a119ae05c",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/236/20/294804188e3d3e5650f0816d90f9ed9d",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/234/20/56d50970ebcc1c71dd6cb57c4965fa39",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/232/20/5c9c702d8e5d89b4b35509303a627b8b",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/228/20/2dafa94b60eebc5572799bb264361b27",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/109/21/7319886f0b5feabb0007c2c8bfe9092d",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/129/21/7ea8df8d1508a361db2f8f3c78591768",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/30/21/2b860791009ec70dfcfb73419c23adc5",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/427/6/38673101281fdedda4b8f3b85c0c2cb2",
#             "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/299/21/44d8b1444a1b0ddd4856abe3f3978df6"
#             )
