# script to format KNZ fish dataset by Keith Gido
library(dplyr)
library(tidyr)
library(popler)

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
                     stringsAsFactors = F)
               

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
              left_join( select(dt1,AbName,ScienceName) ) %>% 
              # remove watersheds with little replication
              subset( !(watershed %in% c('MID','K2A')) ) %>% 
              # take means across 


knz_fish %>% 
  count(year,watershed,habitat)

knz_fish %>% 
  select(year, month) %>% unique

knz_fish %>% 
  select(year, month) %>% 
  unique %>% count( year )

pop_fish %>% head

pop_fish <- pplr_get_data( proj_metadata_key == 763.0 )

pplr_browse( lterid == 'KNZ')
