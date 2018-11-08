# --------------------------------------------------------- #
# Format raw data from L0 into L3 format                                                         #
# Revised 15 May 2018 by ERS                                #
# --------------------------------------------------------- #


# Contributors: Riley Andrade, Max Castorani, Nina Lany, Sydne Record, Nicole Voelker
# revised by Eric Sokol


options(stringsAsFactors = FALSE)
library(testthat)

# Clear environment
rm(list = ls())

# user vars
data_product_directory_name <- 'JRN-677plantDensity-popler'


#Check to make sure working directory is set to the ltermetacommunities github
if(basename(getwd())!="ltermetacommunities"){cat("Plz change your working directory. It should be 'ltermetacommunities'")}

# Check for and install required packages
for (package in c('googledrive','dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom', 'ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# #set up googe drive for R
# ecocom_dp_dir <- googledrive::drive_ls(paste0('~/LTER Metacommunities/LTER-DATA/L0-raw/',
#                                               data_product_directory_name,
#                                               '/ecocomDP_export/'))

# ---------------------------------------------------------------------------------------------------
#IMPORT AND FORMAT DATA FROM EDI

# Package ID: knb-lter-jrn.210351002.75 Cataloging System:https://pasta.edirepository.org.
# Data set title: Jornada Experimental Range permanent quadrat chart data beginning 1915 - plant density.
# Data set creator:  William Chapline -  
# Metadata Provider:    - USDA ARS Jornada Experimental Range (JER) 
# Contact:    - Data Manager Jornada Basin LTER  - datamanager@jornada-vmail.nmsu.edu
# Contact:  Darren James -  USDA ARS Jornada Experimental Range (JER)  - 
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

#read in observations data table
infile1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-jrn/210351002/75/10c1bf759b5700581368e64387c2a347" 
infile1 <- sub("^https","http",infile1) 
main_df <- read.csv( infile1,
                     header=F,
                     skip=1,
                     sep=",",
                     quot='"', 
                     col.names=c( "quadrat",     
                                  "year",     
                                  "month",     
                                  "USDA_code",     
                                  "scientific_name",     
                                  "common_name",     
                                  "duration",     
                                  "form",     
                                  "density"), 
                     check.names=TRUE)
 

#Alternately, read in the cached copy of the data from Google Drive:
#main_df <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/JRN-677plantDensity-popler/archive_knb-lter-jrn/JornadaStudy_351_permanent_chart_quadrat_perennial_forb_density_data.csv")

#sampling location table      
#dt4 <-  read.csv('http://esapubs.org/archive/ecol/E093/132/quad_inventory.csv', stringsAsFactors=F)
dt4       <- read.csv('~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/JRN-677plantDensity-popler/ecocomDP_export/677_location.csv',
                      stringsAsFactors = F) %>%
                mutate( location_name = gsub('quadrat_updated_site_jrn_',
                                             '',
                                             location_name) ) %>%
                subset( location_id %in% c('1_1','1_2','1_7',
                                           '1_8','1_9','1_13') )

# information on location (Aldo added this file to Google Drive. It contains info on coordinates from popler that are not avail on EDI.)
loc_info <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/JRN-677plantDensity-popler/jrn_677_plot_locations.csv",
                      stringsAsFactors = F) %>% 
                .$locs %>% 
                strsplit(' ') %>% unlist %>% 
                matrix( ncol=3, byrow=T ) %>% 
                as.data.frame() %>% 
                setNames( c('location_name', 'lat', 'lon') ) %>% 
                mutate( location_name = tolower(location_name) ) %>% 
                right_join( dt4 ) %>% 
                select(-elevation,-latitude,-longitude) %>% 
                rename( latitude  = lat,
                        longitude = lon )

# quadrat info from ecological archives (http://esapubs.org/archive/ecol/E093/132/). Contains info on quadrat names, elevation, and grazing status.
q_info    <- read.csv('http://esapubs.org/archive/ecol/E093/132/quad_info.csv', 
                      stringsAsFactors = F) %>% 
                mutate( quad.name     = tolower(quad.name) ) %>% 
                # change names ASSUMING these are the correct ones
                mutate( quad.name = replace(quad.name,
                                            quad.name == 'a1p',
                                            'a1') ) %>% 
                mutate( quad.name = replace(quad.name,
                                            quad.name == 'a2p',
                                            'a2') ) %>% 
                mutate( quad.name = replace(quad.name,
                                            quad.name == 'b3p',
                                            'b3') ) %>% 
                mutate( quad.name = replace(quad.name,
                                            quad.name == 'b4p',
                                            'b4') ) %>% 
                mutate( quad.name = replace(quad.name,
                                            quad.name == 'b5p',
                                            'b5') ) %>% 
                mutate( quad.name = replace(quad.name,
                                            quad.name == 'sg4',
                                            'g4') ) %>% 
                rename( location_name = quad.name) %>% 
                select(location_name, elevation, grazing.status,
                       grazing.notes.prior.to.time.span.of.chart,
                       grazing.notes.during.time.span.of.chart ) %>% 
                right_join( loc_info )
 

# 4 Make SPATIAL_COORDINATE from the sampling location table (dt4)
# select the columns of interest and convert to long 
lat_lon <- q_info %>% 
              select(location_id, latitude,longitude) %>% 
              gather('VARIABLE_NAME', 'VALUE', latitude, longitude) %>%
              rename(SITE_ID = location_id) %>%
              mutate(OBSERVATION_TYPE = 'SPATIAL_COORDINATE',
              VARIABLE_UNITS = 'dec. degrees',
              DATE = NA) %>% 
              select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE)

# grazing info
graz_info <- q_info %>% 
              select(location_id, grazing.status) %>% 
              mutate(OBSERVATION_TYPE = 'Site_condition',
                     VARIABLE_NAME    = 'grazing_status',
                     VARIABLE_UNITS   = NA,
                     DATE             = NA ) %>% 
              rename(SITE_ID = location_id,
                     VALUE   = grazing.status) %>%
              select(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE)

      
# # get locations by directly querying popler
# conn      <- popler:::db_open()
# dt4       <- popler:::query_get(conn,"SELECT * FROM study_site_table") %>%
#                 subset( grepl('jrn', study_site_key )) %>%
#                 mutate( study_site_key = paste0('quadrat_updated_', study_site_key) ) %>%
#                 rename( location_name = study_site_key ) %>%
#                 right_join( read.csv('C:/677_location.csv') ) %>%
#                 select(-latitude, -longitude,-descript) %>%
#                 rename( latitude  = lat_study_site,
#                         longitude = lng_study_site )


# Fix taxa mistakes
# google_id <- ecocom_dp_dir %>% filter(grepl('taxon',name)) %>% select(id) %>% unlist()
# dt5 <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id)) %>% 
# dt5     <- read.csv('C:/677_taxon.csv') %>% 
main_tax <- main_df %>% 
              # fix mistake in the name of a species 
              mutate( scientific_name = gsub('Hoffmanseggia','Hoffmannseggia', scientific_name) ) %>% 
              mutate( scientific_name = replace(scientific_name, 
                                                scientific_name == 'Opuntia santa rita',
                                                'Opuntia santarita') ) %>%
              mutate( scientific_name = replace(scientific_name, 
                                                scientific_name == 'Ammocodon chenopodiodes',
                                                'Ammocodon chenopodioides') ) %>%
              # remove unknown species
              subset( !grepl('Unknown', scientific_name) ) %>% 
              # homogenize how genus-level identification is "flagged" ('spp.' instead of 'species')
              mutate( scientific_name = gsub("species","spp.",scientific_name) ) %>% 
              # fix mistakes in taxon_id
              mutate( USDA_code = replace(USDA_code, USDA_code == 'SPHA', 'SPHAE') ) %>% 
              # fix mistake with Spheraclea. I call 'hastulata' as. 'spp.' because in this genus
              # level ID is predominant (see below)
              mutate( scientific_name = replace( scientific_name, 
                                                 scientific_name == 'Sphaeralcea hastulata',
                                                 'Sphaeralcea spp.') )


# Take means of density by year
# select the columns of interest
# dat1 <- dt1 %>%
jrn <- main_tax %>%
          group_by(quadrat, year, USDA_code, scientific_name) %>% 
          # take average of replicate observations in a given location and time
          summarize(value = mean(density, na.rm = TRUE)) %>%
          ungroup() %>%
          rename(SITE_ID          = quadrat, 
                 DATE             = year,
                 VARIABLE_NAME    = USDA_code,
                 VALUE            = value) %>% 
          mutate(OBSERVATION_TYPE = 'TAXON_COUNT',
                 VARIABLE_UNITS   = 'count (average)' ) %>%
          select(OBSERVATION_TYPE,
                 SITE_ID, 
                 DATE, 
                 VARIABLE_NAME, 
                 VARIABLE_UNITS, 
                 VALUE) #%>% 
          # right_join( rename(dt5, VARIABLE_NAME = taxon_id) ) %>% 
          # select( -taxon_rank, -taxon_name, -authority_system, -authority_taxon_id)


# 3. resolve issues with genus-level IDs -------------------------------------------------------
  
# get genuses where observations were identified at the genus level
genus_ids <- main_tax %>% 
                select( scientific_name ) %>% 
                mutate( scientific_name = gsub("species","spp.",scientific_name) ) %>% 
                subset( grepl('spp\\.|species', scientific_name) ) %>% 
                separate( scientific_name, into = c('genus', 'spp'), sep =" " ) %>% 
                unique

# Test: codes of taxa identified at the genus level correspond to a unique 'scientific_name'
main_tax %>% 
  mutate( scientific_name = gsub("species","spp.",scientific_name) ) %>% 
  subset( grepl('spp.', scientific_name) ) %>% 
  select(USDA_code, scientific_name) %>% 
  unique %>% 
  .$USDA_code %>% 
  table %>% 
  unique %>% 
  expect_equal(1)

# get the genuses to lump (those that are identified at the species level AS WELL)
codes_df <- main_tax %>% 
              select( USDA_code, scientific_name ) %>% 
              unique %>% 
              subset( grepl(paste(genus_ids$genus,collapse="|"), 
                            scientific_name) ) %>% 
              separate( scientific_name, into = c('genus', 'species'), sep =" " ) %>% 
              group_by( genus ) %>% 
              summarise( rep=n() ) %>%
              # select IDs identified at genus AND species level
              subset( rep > 1 ) %>% 
              ungroup %>% 
              .$genus %>% 
              lapply(function(x) subset(main_tax, grepl(x,scientific_name) ) )

# calculate the proportion density measurements at the genus-level
prop_gen <- function(x){
  
  # transform taxonomic data in order to merge it to the (formatted) observation data frame
  x <- x %>% 
        rename(VARIABLE_NAME = USDA_code) %>% 
        select(VARIABLE_NAME, scientific_name) %>% 
        unique
  
  # calculate proportions of counts for each ID category
  # use this to calculate proportion of counts IDed to genus rather than species
  # dat1 %>% 
  jrn %>% 
    subset( grepl(paste(x$VARIABLE_NAME,collapse="|"), 
                        VARIABLE_NAME) ) %>% 
    group_by(VARIABLE_NAME) %>% 
    summarise( total = sum(VALUE) ) %>% 
    arrange( total ) %>% 
    mutate( prop = total/sum(total) ) %>% 
    arrange( desc(prop) ) %>% 
    as.data.frame %>% 
    left_join( x )
  
}

# proportion of species identified to genus level
prop_l <- lapply(codes_df, prop_gen) 

# remove genus level IDed that make up less than 5% of counts in a genus
r_gen  <- prop_l %>% 
            lapply( function(x){ 
                      out <- subset(x, grepl('spp\\.', scientific_name))
                      if(out$prop < 0.05) return(T) else return(F)} 
                  ) %>% 
            unlist %>% 
            which

# Lump entities to the genus level 
# because more than 5% of counts within a genus cannot not be identified at the species level
l_gen  <- prop_l %>% 
            lapply( function(x){ 
                      out <- subset(x, grepl('spp\\.', scientific_name))
                      if(out$prop >= 0.05) return(T) else return(F)} 
                  ) %>% 
            unlist %>% 
            which

# 3A. remove IDS of species identified to genus level only.
r_codes<- prop_l[r_gen] %>% 
            # select genus-level data (flagged by "spp.")
            lapply( function(x) subset(x, grepl('spp\\.', scientific_name)) ) %>% 
            # re-combine into a data frame
            Reduce( function(...) rbind(...), .) %>% 
            # get codes to remove
            .$VARIABLE_NAME

# test that you've not lost any identifier
expect_true( identical(1:length(prop_l), c(r_gen,l_gen) %>% sort ) )

# 3B. lump IDs to the genus level
lump_spp <- function(x){
  
  mutate(x, new_var = subset(x, grepl('spp\\.', scientific_name))$VARIABLE_NAME ) %>% 
    select(-total, -prop, -scientific_name)
  
}

# new variables (that lump individuals at the genus level)
newvar_df <- lapply(prop_l[l_gen], lump_spp) %>% 
                Reduce(function(...) rbind(...), .) %>% 
                subset( !(VARIABLE_NAME == new_var) )


# update taxonomic IDs
count_d   <- jrn %>% 
                # 3A. remove IDS of species identified to genus level only. 
                subset( !(VARIABLE_NAME %in% r_codes) ) %>% 
                # 3B. lump IDs to the genus level
                left_join( newvar_df ) %>% 
                # substitute VARIABLE_NAME with new_var only if new_var IS NOT an NA.
                mutate( VARIABLE_NAME = plyr::mapvalues(VARIABLE_NAME,
                                                        from = newvar_df$VARIABLE_NAME,
                                                        to   = newvar_df$new_var )

                        ) %>% 
                dplyr::select( -new_var ) %>% 
                
                # take means AGAIN: lumped taxonomy need be 
                group_by(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS) %>% 
                summarise( VALUE = mean(VALUE, na.rm=T) ) %>% 
                ungroup() %>% 
  
                # introduce zeros
                spread(key = VARIABLE_NAME, value = VALUE, fill = 0) %>% 
                gather(key = VARIABLE_NAME, value = VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS) %>% 
                
                # 4. select sites and year BY HAND
                subset( SITE_ID %in% c( 'a1', 
                                        'a2',
                                        'a4',
                                        'a2',
                                        'b4',
                                        'b5',
                                        'g4',
                                        'g5',
                                        'i1',
                                        'i2',
                                        'i4') ) %>% 
                subset( DATE < 1939 ) %>% 
                subset( !(DATE %in% c(1918,1922,1926,1929,1930,1934)) ) 

# OVERKILL: test that you did substitude characters correctly
test_post  <- jrn %>% 
                # 3A. remove IDS of species identified to genus level only. 
                subset( !(VARIABLE_NAME %in% r_codes) ) %>% 
                # 3B. lump IDs to the genus level
                left_join( newvar_df )  %>% 
                # substitute VARIABLE_NAME with new_var only if new_var IS NOT an NA.
                mutate( VARIABLE_NAME = plyr::mapvalues(VARIABLE_NAME,
                                                        from = newvar_df$VARIABLE_NAME,
                                                        to   = newvar_df$new_var )

                        ) %>% 
                select( -new_var )
test_pre <- subset(jrn,!(VARIABLE_NAME %in% r_codes) )

# perform the actual tests
expect_equal( nrow(test_pre), nrow(test_post) )
for(ii in 1:nrow(newvar_df)){
  expect_true( (which(test_post$VARIABLE_NAME == newvar_df$VARIABLE_NAME[ii]) %in% 
                  which(test_pre$VARIABLE_NAME == newvar_df$new_var[ii]) ) %>% all )
}

# combine data into one dataframe
out <- Reduce(function(...) rbind(...), 
              list(count_d,lat_lon, graz_info) )

write.csv(out, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-jrn-plants-compagnoni.csv", row.names = F)    


# 
# #4 Combine into one big table
# # dat.long <- rbind(dat1, dat4)
# 
# # #5 perform a few checks against dataset summary table (dt3)
# # ifelse(length(unique(dat1$VARIABLE_NAME))==dt3$max_num_taxa,
# #   "OK: Number of taxa matches dataset summary table.",
# 2#   "ERROR:Number of taxa does not match dataset summary table.")
# 
# # date <- as.POSIXlt(dat1$DATE, format = "%Y-%m-%d")
# # year <- as.numeric(format(date, "%Y"))
# # ifelse(length(unique(year))== dt3$number_of_years_sampled, 
# #   "OK: Number of years sampled matches dataset summary table.",
# #   "ERROR:Number of years sampled does not match dataset summary table.")
# 
# # ifelse( max(year)-min(year) + 1 == dt3$length_of_survey_years, 
# #   "OK: Length of survey matches dataset summary table.",
# #   "ERROR:Length of survey does not match dataset summary table.")
# 
# #
# 
# # MAKE DATA LIST
# dat <- list()
# 
# # COMMUNITY DATA 
# comm.long <- count_d
# 
# # checking for weird taxa names and removing suspicious ones that are rare
# comm.long_gamma_summary <- comm.long %>% 
#   group_by(VARIABLE_NAME) %>% 
#   filter(!is.na(VARIABLE_NAME) & VALUE>0) %>%
#   summarize(mean_value = mean(VALUE, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(RA = mean_value / sum(mean_value))
# 
# comm.long_gamma_summary_remove_unk <- comm.long_gamma_summary %>% 
#   filter(!grepl('(?i)unk',VARIABLE_NAME))
# 
# if(!nrow(comm.long_gamma_summary) == nrow(comm.long_gamma_summary_remove_unk)){
#   message('WARNING: suspicous taxa removed -- taxaID had "unk"')
# }
# 
# 
# # Subset data if necessary
# #comm.long <- subset(comm.long, comm.long$TAXON_GROUP != "INSERT NAME OF REMOVAL GROUP HERE")
# #comm.long <- droplevels(comm.long)
# str(comm.long)  # Inspect the structure of the community data
# 
# #Add number of unique taxa and number of years to data list:
# dat$n.spp <- length(levels(comm.long$VARIABLE_NAME))
# 
# # Ensure that community data VALUE and DATE are coded as numeric
# # if more than one date per year, aggregate by taking mean counts/biomass/cover across bouts per year.
# comm_sampling_summary_table <- comm.long %>%
#   mutate(YEAR = DATE ) %>%
#   # mutate(YEAR = lubridate::year(DATE)) %>%
#   group_by(SITE_ID, YEAR) %>%
#   summarize(n_bouts_year = length(unique(DATE))) %>%
#   rename(DATE = YEAR)
# 
# if(length(unique(comm_sampling_summary_table$n_bouts_year)) > 1){
#   message('WARNING: different number of sampling bouts in each different years!')
#   print(comm_sampling_summary_table)
# }
# 
# dat$comm_sampling_summary_table <- comm_sampling_summary_table
# 
# comm.long <- comm.long %>%   # Recode if necessary
#   mutate(DATE = DATE ) %>%
#   # mutate(DATE = lubridate::year(DATE)) %>%
#   mutate_at(vars(c(DATE, VALUE)), as.numeric) %>%
#   group_by(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS) %>%
#   summarize(VALUE = mean(VALUE, na.rm = TRUE),
#             n_bouts = length(DATE)) %>%
#   select(-n_bouts) %>%
#   ungroup()
# 
# dat$n.years <- length(unique(comm.long$DATE))
# 
# # Ensure that community character columns coded as factors are re-coded as characters
# comm.long <- comm.long %>%   # Recode if necessary
#   mutate_if(is.factor, as.character)
# 
# # Ensure that SITE_ID is a character: recode numeric as character 
# comm.long <- comm.long %>%   # Recode if necessary
#   mutate_at(vars(SITE_ID), as.character)
# 
# # Double-check that all columns are coded properly
# ifelse(FALSE %in% 
#          c(
#            class(comm.long$OBSERVATION_TYPE) == "character",
#            class(comm.long$SITE_ID) == "character",
#            class(comm.long$DATE) == "numeric",
#            class(comm.long$VARIABLE_NAME) == "character",
#            class(comm.long$VARIABLE_UNITS) == "character",
#            class(comm.long$VALUE) == "numeric"
#            #class(comm.long$TAXON_GROUP) == "character")
#          ),
#        "ERROR: Community columns incorrectly coded.", 
#        "OK: Community columns correctly coded.")
# 
# # ---------------------------------------------------------------------------------------------------
# # Check balanced sampling of species across space and time by inspecting table, and add to data list
# 
# # make wide, fill with 0, then back to long to propagate zeros
# if(length(unique(xtabs(~ SITE_ID + DATE, data = comm.long))) > 1){
#   comm.long <- comm.long %>%
#     spread(VARIABLE_NAME, VALUE, fill = 0) %>%
#     gather('VARIABLE_NAME','VALUE', -c(OBSERVATION_TYPE, SITE_ID, DATE, VARIABLE_UNITS))
# }
# 
# xtabs(~ SITE_ID + DATE, data = comm.long)
# hist(na.omit(comm.long$DATE))
# 
# ifelse(length(unique(xtabs(~ SITE_ID + DATE, data = comm.long))) == 1,
#        "OK: Equal number of taxa recorded across space and time.", 
#        "ERROR: Unequal numbers of observations across space and time, or taxa list not fully propagated across space and time. Inspect contingency table.")
# 
# # ---------------------------------------------------------------------------------------------------
# # Add to dat list the unique taxa
# dat$comm.long <- comm.long
# 
# # Convert community data to wide form
# comm.wide <- comm.long %>%
#   select(-VARIABLE_UNITS) %>%
#   spread(VARIABLE_NAME,  VALUE)
# 
# dat$comm.wide <- comm.wide
# summary(dat)
# 
# dat.long <- comm.long
# 
# # ---------------------------------------------------------------------------------------------------
# # SPATIAL DATA
# # Check for and install required packages
# if( nrow(dat4) > 0 & !(dat4$VALUE %>% anyNA()) ){
#   for (package in c('dplyr', 'tidyr', 'XML', 'sp', 'geosphere', 'rgdal','maps','reshape2','ggplot2')) {
#     if (!require(package, character.only=T, quietly=T)) {
#       install.packages(package)
#       library(package, character.only=T)
#     }
#   }
#   
#   dat.long <- bind_rows(dat.long, dat4)
#   
#   #pull out coordinate data and make sure that it is numeric
#   cord <- filter(dat.long, OBSERVATION_TYPE=="SPATIAL_COORDINATE");head(cord)
#   cord$SITE_ID <- toupper(cord$SITE_ID)  # Ensure sites are in all caps
#   cord <- droplevels(cord)
#   str(cord)
#   
#   cord.wide <- cord %>%
#     select(-VARIABLE_UNITS) %>%
#     spread(VARIABLE_NAME,  VALUE)
#   
#   head(cord.wide)
#   
#   sites <- c(unique(cord.wide$SITE_ID));sites
#   
#   # keep the records that are _not_ duplicated
#   cord.wide <- subset(cord.wide, !duplicated(SITE_ID));dim(cord.wide)  # here we selcet rows (1st dimension) that are different from the object dups2 (duplicated records)
#   cord.wide
#   cord.wide$latitude <- as.numeric(as.character(cord.wide$LAT)) 
#   cord.wide$longitude <- as.numeric(as.character(cord.wide$LONG)) #cord.wide$longitude <- as.numeric(as.character(cord.wide$longitude))
#   cord.wide <- cord.wide[c("longitude", "latitude")] #pull last two columns and reorder
#   
#   #add number of sites and long/lat coords to data list:
#   dat$n.sites <- length(sites)
#   dat$longlat <- cord.wide
#   
#   #make data spatially explicit
#   coordinates(cord.wide) = c("longitude", "latitude") #coordinates(cord.wide) <- c("longitude", "latitude") 
#   crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # SBC
#   crs.geo <- CRS("+proj=utm +zone=13 +datum=WGS84") #NWT, PHX=zone12
#   proj4string(cord.wide) <- crs.geo  # define projection system of our data to WGS84 (CHECK TO SEE IF THIS WORKS IF SPATIAL COORDINATE IS NOT IN DEC.DEGREES)
#   summary(cord.wide) 
#   
#   #if DATA IS IN UTM OR OTHER KNOWN COORDINATE SYSTEM YOU CAN TRANSFORM IT, EG... UTM data for PHX and NWT 
#   cord.wide <- spTransform(cord.wide, CRS("+proj=longlat")) 
#   summary(cord.wide) #check transformation
#   
#   #create a distance matrix between sites, best fit distance function TBD
#   distance.mat <- (distm(cord.wide, fun = distVincentyEllipsoid)/1000);distance.mat #km distance
#   rownames(distance.mat) <- sites
#   colnames(distance.mat) <- sites
#   
#   #add distance matrix to data list
#   dat$distance.mat <- distance.mat
#   summary(dat)
# }else{
#   
#   dat$n.sites <- dat.long$SITE_ID %>% unique() %>% length()
#   message('WARNING: spatial data missing')
#   
# }
# 
# 
# # ---------------------------------------------------------------------------------------------------
# # ENVIRONMENTAL COVARIATES
# if( 'ENV_VAR'%in%dat.long$OBSERVATION_TYPE & !(dat.long %>% filter(OBSERVATION_TYPE == 'ENV_VAR'))$VALUE %>% anyNA() ){
#   
#   env.long <- subset(dat.long, OBSERVATION_TYPE == "ENV_VAR")
#   env.long <- droplevels(env.long)
#   str(env.long)
#   
#   # Convert from long to wide
#   env.wide <- env.long %>%
#     select(-VARIABLE_UNITS) %>%
#     tidyr::spread(VARIABLE_NAME,  VALUE)
#   
#   # Add environmental covaiates to data list
#   
#   dat$n.covariates <- length(levels(env.long$VARIABLE_NAME))
#   dat$cov.names <- levels(env.long$VARIABLE_NAME)
#   dat$env <- env.wide
#   dat$env.long <- env.long
#   
#   #CHECK: 
#   # Are all year-by-site combinations in community data matched by environmental data?
#   ifelse(nrow(dat$comm.wide) == nrow(dat$env), "Yes", "No")
#   
# }else{
#   message('WARNING: environmental data missing')
# }
# 
# 
# 
# 
# # Are community data balanced over space and time?
# ifelse(nrow(dat$comm.wide) == dat$n.years * dat$n.sites, "Yes", "No")
# # Inspect summary of 'dat' list 
# summary(dat)
# 
# # #clean up the workspace
# # try({
# #   rm("comm.long","comm.wide","cord","cord.wide","crs.geo","dat.long", "data.key", "data.set","distance.mat","env.long", "env.wide","package","sites")
# #   ls()
# # })
# 
# 
# # Now, explore the data and perform further QA/QC by sourcing this script within the scripts "2_explore_spatial_dat.R", "3_explore_comm_dat.R", and "4_explore_environmental_dat.R"
# 
# # Write CSV file for cleaned data (L3)
# # write.csv(out_long, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-hbr-birds-sillett.csv", row.names = F)
# write_path <- '~/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/'
# drive_ls(write_path)
# 
# write_filename <- paste0('L3-',tolower(data_product_directory_name),'.csv')
# 
# # temp write local
# readr::write_csv(dat.long, write_filename)
# drive_upload(write_filename, 
#              path = write_path, 
#              name = write_filename, 
#              type = NULL,
#              verbose = TRUE)
# 
# #remove local file
# file.remove(write_filename)
# 
