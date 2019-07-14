# Aldo Compagnoni Nov 2018
#modified by NKL 03/25/2019
rm(list = ls())

library(dplyr)
library(tidyr)


#Data are from Ecologcial Archives
## http://esapubs.org/archive/ecol/E088/161/
##Peter B. Adler, William R. Tyburczy, and William K. Lauenroth. 2007. Long-term mapped quadrats from Kansas prairie: demographic information for herbaceaous plants. Ecology 88:2673.
#Downloaded by to the L0 diretory in the Google Drive by NKL on Nov 16, 2018. 


hays <- read.csv('http://esapubs.org/archive/ecol/E088/161/allrecords.csv', stringsAsFactors=F) 
spp_list <- read.csv('http://esapubs.org/archive/ecol/E088/161/species_list.csv', stringsAsFactors=F) 
quad_info <- read.csv('http://esapubs.org/archive/ecol/E088/161/quadrat_info.csv', stringsAsFactors=F) 
quad_inv <- read.csv('http://esapubs.org/archive/ecol/E088/161/quadrat_inventory.csv', stringsAsFactors=F)


# read files from my own (Aldo's) machine (can't do it online)
#hays     <- read.csv('C:/allrec_hays.csv') 
#spp_list <- read.csv('C:/species_list.csv')
#quad_info<- read.csv('C:/quadrat_info.csv')
#quad_inv <- read.csv('C:/quadrat_inventory.csv')
  
  
#read in files from Google Drive:
#hays <- read.csv('~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/HAYS-plants/allrecords.csv') 
#spp_list <- read.csv('~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/HAYS-plants/species_list.csv')
#quad_info<- read.csv('~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/HAYS-plants/quadrat_info.csv')
#quad_inv <- read.csv('~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/HAYS-plants/quadrat_inventory.csv')
  

# hays raw
hays_raw <- hays %>% 
              mutate( plotyear = as.character(plotyear) ) %>% 
              # rename plot that messes with pattern
              mutate( plotyear = replace(plotyear,
                                         grepl( 'e2qo-10',plotyear),
                                         'e2qo-X')) %>% 
              separate( plotyear, c('plot','year'), sep='-') %>% 
              mutate( plot_n = substr(year,1,1) ) %>% 
              mutate( year   = substr(year,2,3) ) %>% 
              # re-rename plot that messed with pattern
              mutate( plotyear = replace(plot,
                                         plot == 'e2qo-X',
                                         'e2qo-10')) %>% 
              mutate( plot = paste(plot,plot_n,sep='-') ) %>% 
              # count species by year/plot
              count(plot,year,species) %>% 
              # remove non-plants and unknowns
              subset( !(species %in% c('Unknown.',
                                       'Unknown',
                                       'Short grass',
                                       'Fragment',
                                       'Bare ground')) )

# visually examine items with unclear taxonomic ID
hays_raw %>% 
  group_by(species) %>% 
  summarise(all_n = sum(n) ) %>% 
  subset(grepl(spp_gen,species))

# genera to remove (less than 5% of all counts)
r_gens <- c('Ambrosia spp.','Oxalis spp.', 'Solidago spp.')
# genera to lump (species identified lass than 95% of the time)
lump_gens <- c('Allium', 'Chamaesyce', 'Opuntia', 'Polygala')
         
# final hays data frame    
hays_out <- hays_raw  %>% 
              # remove a few individuals identified at the genera level
              subset( !(species %in% r_gens) ) %>%
              # "lump" gens. iddentified at spp. level less than 95% of counts
              mutate( species = replace(species,
                                        grepl('Allium',species),
                                        'Allium spp.') ) %>% 
              mutate( species = replace(species,
                                        grepl('Chamaesyce',species),
                                        'Chamaesyce spp.') ) %>%
              mutate( species = replace(species,
                                        grepl('Opuntia',species),
                                        'Opuntia spp.') ) %>% 
              mutate( species = replace(species,
                                        grepl('Opuntia',species),
                                        'Opuntia spp.') ) %>% 
              # re-count individuals 
              group_by(plot,year,species) %>% 
              summarise( count = sum(n) ) %>% 
              ungroup %>% 
              rename(SITE_ID = plot, 
                   DATE = year,
                   VARIABLE_NAME = species,
                   VALUE = count) %>%
              mutate(VARIABLE_UNITS = 'COUNT',
                     OBSERVATION_TYPE = 'TAXON_COUNT') %>%
              select(OBSERVATION_TYPE,SITE_ID, DATE, 
                     VARIABLE_NAME, VARIABLE_UNITS, VALUE) %>% 
              # insert zeros
              spread(key = VARIABLE_NAME, value = VALUE, fill = 0) %>% 
              gather(key = VARIABLE_NAME, value = VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS) 
      
# keep plots with continuous rep
keep_plot <- hays_out %>% 
              mutate( DATE = as.numeric(DATE) ) %>% 
              subset( DATE > 37 & DATE < 73) %>% 
              select(SITE_ID,DATE) %>% 
              unique %>% 
              count(SITE_ID) %>% 
              subset( n == 35) %>% 
              .$SITE_ID

# write it out
hays_out %>% 
  subset( SITE_ID %in% keep_plot) %>% 
  subset( DATE >37 ) %>% 

write.csv('C:/L3-hays-compagnoni.csv',row.names=F)  


# Lauren Hallett subset ---------------------------

keep_hallett <- grep(paste(keep_plot,collapse='|'),
      hays_out$SITE_ID,value=T) %>% unique

quad_info %>% 
  subset( quadrat %in% keep_hallett ) 
  
hays_out %>% 
  subset( SITE_ID %in% keep_hallett ) %>% 
  subset( DATE > 42 & DATE < 73 ) %>% 
  subset( !(VALUE %in% 0) ) %>% 
  .$VARIABLE_NAME %>% unique

# remove super-rare species ---------------------------------------

rare_spp <- spp_list %>% subset( count == 1) %>% .$species

L3dat <- hays_out %>% 
          subset( SITE_ID %in% keep_plot) %>% 
          subset( DATE >37 ) %>% 
          subset( !(VARIABLE_NAME %in% rare_spp) )

  


# Check that taxa do not change through time ----------------------

hays_out %>% 
  subset( SITE_ID %in% keep_plot) %>% 
  subset( DATE >37 ) %>% .$VARIABLE_NAME %>% 
  unique %>% sort
  
hays_out %>% 
  subset( VARIABLE_NAME == "Bromus japonicus") %>% 
  select(SITE_ID,DATE,VALUE) %>% 
  subset( VALUE > 0 ) %>% 
  .$VALUE %>% length
  ggplot() +
  geom_line( aes(x = DATE, y = VALUE, colour=SITE_ID) ) +
  theme(legend.position = 'none')

# Can we get sites ------------------------------------------------
quad_info %>% 
  mutate( quadrat=as.character(quadrat)) %>% 
  right_join( data.frame( quadrat = keep_plot,
                          stringsAsFactors=F) ) %>% 
  .$group %>% table

# all available groups
quad_info %>% .$group %>% table


sel_site <- quad_info %>% 
              subset( group == 'sg3') %>% 
              .$quadrat %>% 
              as.character
  
sel_c <- gsub('-','.',sel_site) %>% 
  setdiff( c('e2qo.1', 'e2qo.2', 'e2qo.5', 'e2qo.6') )

quad_inv[c(5:23),sel_c] 

L3dat <- hays_out %>% 
  mutate( DATE = as.numeric(DATE) ) %>% 
  subset( SITE_ID %in% gsub('\\.','-',sel_c )) %>% 
  subset( DATE %in% c(36:54) ) #%>% 
  write.csv('C:/L3-hays-compagnoni-homogeneus.csv',row.names=F)  
  
# spp_list %>%  .$count %>% table

# write.csv('~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-hays-plants-compagnoni.csv',row.names=F)  

