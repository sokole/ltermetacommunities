# script to format KNZ fish dataset by Keith Gido
library(dplyr)
library(tidyr)
library(popler)

# Package ID: knb-lter-knz.87.6 Cataloging System:https://pasta.edirepository.org.
# Data set title: CFP01 Fish population on selected watersheds at Konza Prairie.
# Data set creator:  Keith Gido -  
# Metadata Provider:    - Konza LTER 
# Contact:  Konza LTER -    - knzlter@ksu.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 



# first file
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/87/6/2279085bc3c7964149330c2dd8d7e284" 
infile1   <- sub("^https","http",infile1) 
dt1       <-read.csv(infile1,header=F,
                     skip=1,
                     sep=",",
                     quot='"', 
                     col.names=c( "DataCode",     
                                  "Rectype",     
                                  "ListID",     
                                  "AbName",     
                                  "CommonName",     
                                  "ScienceName",     
                                  "Comments"    ), 
                     check.names=TRUE,
                     stringsAsFactors = F) %>% 
                # include unknowns in "ScienceName" column
                mutate( ScienceName = replace(ScienceName,
                                              AbName == 'CRAY',
                                              'Unidentified crayfish') ) %>% 
                mutate( ScienceName = replace(ScienceName,
                                              AbName == 'TADPOLE',
                                              'Unidentified tadpole') ) 
  
               

# second file
infile2   <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/87/6/3ebcf7b1e8ae47eb2877e4db036f3e3e" 
infile2   <- sub("^https","http",infile2) 
dt2       <-read.csv(infile2,header=F,
                     skip=1,
                     sep=",",
                     quot='"', 
                     col.names=c( "datacode",     
                                  "rectype",     
                                  "recdate",     
                                  "watershed",     
                                  "habitat",     
                                  "replicate",     
                                  "CAMANO",     
                                  "CATCOM",     
                                  "CRAY",     
                                  "CYPLUT",     
                                  "ETHNIG",     
                                  "ETHSPE",     
                                  "GAMAFF",     
                                  "LEPCYA",     
                                  "LEPHUM",     
                                  "LEPMAC",     
                                  "LEPMEG",     
                                  "LUXCOR",     
                                  "NOTEXI",     
                                  "NOTPER",     
                                  "NOTSTR",     
                                  "ORCNAI",     
                                  "ORCNEG",     
                                  "PHEMIR",     
                                  "PHOERY",     
                                  "PIMNOT",     
                                  "PIMPRO",     
                                  "SEMATR",     
                                  "TADPOLE"    ), check.names=TRUE,
                     stringsAsFactors = F)
               

# format data 
knz_fish <- dt2 %>% 
              gather( AbName, count, CAMANO:TADPOLE ) %>% 
              separate( recdate, c('month','day','year') ) %>% 
              # combine with scientific names
              left_join( select(dt1, AbName, ScienceName) ) %>% 
              # remove not needed columns
              select( -datacode, -rectype, -day ) %>% 
              # remove watersheds with little replication
              subset( !(watershed %in% c('MID','K2A')) ) %>%
              # remove "combined" habitat
              subset( !(habitat %in% 'combined') ) %>% 
              # select only seasons where 
              subset( month %in% c(8,9,11,12) ) %>% 
              # take sums across years
              group_by( year, watershed, habitat, replicate, ScienceName) %>% 
              summarise( count = sum(count) ) %>% 
              ungroup %>% 
              # combine spatial replicates
              mutate( SITE_ID = paste(watershed, habitat, replicate, sep = '_') ) %>% 
              # remove originals site information
              select( -watershed, -habitat, -replicate ) %>% 
              # rename variables according to LTERmetacommunities data standard
              rename( DATE             = year,
                      VARIABLE_NAME    = ScienceName,
                      VALUE            = count ) %>% 
              mutate( OBSERVATION_TYPE = 'TAXON_COUNT',
                      VARIABLE_UNITS   = 'COUNT (summer + fall)' ) %>% 
              # order variables
              select( OBSERVATION_TYPE, 
                      SITE_ID, 
                      DATE, 
                      VARIABLE_UNITS, 
                      VARIABLE_NAME, 
                      VALUE) %>% 
              # propagate zeros (just in case)
              spread(VARIABLE_NAME, VALUE, fill = 0) %>% 
              gather(VARIABLE_NAME, VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS) %>% 
              # remove 
              subset( !(VARIABLE_NAME %in% c("Unidentified crayfish",
                                             "Unidentified tadpole")) ) %>% 
              # select only sites with high replication
              subset( SITE_ID %in% c('NT_RIFFLE_1', 'NT_RIFFLE_2', 
                                     'NT_POOL_1',   'NT_POOL_2', 
                                     'N4D_POOL_1', 'N4D_POOL_2', 
                                     'N4D_POOL_3', 'N1B_POOL_1', 
                                     'N1B_POOL_2') )
              
       
# ggplot2::ggplot(data = knz_fish,
#                 ggplot2::aes(x = DATE,
#                              y = SITE_ID) ) +
#     ggplot2::geom_point(size = 5) +
#     ggplot2::theme_bw() + 
#     ggplot2::xlab("Year with available data") + 
#     ggplot2::ylab("Site")

       	
# store formatted file
write.csv(knz_fish, "~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-knz-fish-compagnoni.csv", row.names=F)

