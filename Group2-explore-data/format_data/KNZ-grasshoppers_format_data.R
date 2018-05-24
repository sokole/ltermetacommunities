rm(list = ls(all = T))
options(stringsAsFactors = F)
library(dplyr)
library(tidyr)
library(stringi)

# read raw data: information on sites, and information on species counts
site_df   <- read.csv("C:/KNZ-grasshoppers_sites.csv") 
count_df  <- read.csv("C:/KNZ-grasshoppers_counts.csv")

# remove DATACODE, RECTYPE: they will impede a "join". Plus, they are undefined in metadata
site_df  <- site_df %>% 
              setNames( toupper(names(.)) ) %>% 
              select(-DATACODE,-RECTYPE)

count_df <- count_df %>% 
              setNames( toupper(names(.)) ) %>% 
              select(-DATACODE,-RECTYPE)

# check "SPCODE" versus taxonomic information


# merges data sets, gathers sweep data from b, replaces * in COUNT with NA
knz <- left_join(count_df, site_df) %>% 
          gather(SWEEP, COUNT, S1:S10) %>% 
          mutate(COUNT = replace(COUNT, COUNT == '*', NA) ) %>% 
          # also replace "" with NA 
          mutate(COUNT = replace(COUNT, COUNT == '', NA) ) %>% 
          # separates SPECIES into GENUS and SPECIES
          separate(SPECIES, into = c("genus","species"), sep = " ") %>% 
          # remove every unknown
          subset( !grepl('unkn',genus, ignore.case=T) ) %>% 
          # capitalize first letter
          mutate( genus = stri_trans_totitle(genus) ) %>% 
          # introduce NAs when SPECIES ==""
          mutate( species = replace(species, species == "", NA) ) %>% 
          # removes all NA in dates (unusable data)
          filter( !is.na(RECDAY) ) %>% 
  
          # FIX TAXONOMY
          # clear mistakes
          mutate( species = replace(species, genus == 'Syrbula' & species == 'admirabil',
                                    'admirabilis'),
                  genus   = replace(genus, genus == 'Schistocer', 'Schistocerca') ) %>%
          mutate( genus = replace(genus, 
                                  genus == 'Phoetaliot' & species == 'nebrascen',
                                  'Phoetaliotes') ) %>% 
          mutate( species = replace(species, 
                                    genus == 'Phoetaliotes' & species == 'nebrascen',
                                    'nebrascensis') ) %>% 
          mutate( genus = replace(genus, genus == 'Paratylotr', 'Paratylota') ) %>% 
          mutate( species = replace(species, 
                                    genus == 'Phoetaliot' & species == 'nebrascen',
                                    'Phoetaliotes') ) %>%
          mutate( species = replace(species, 
                                    genus == 'Melanoplus' & species == 'femurrubr',
                                    'femurrubrum') ) %>%
          mutate( genus = replace(genus, genus == 'Hadrotetti', 'Hadrotetti') ) %>% 
          mutate( species = replace(species, 
                                    genus == 'Boopedon' & species == 'auriventr',
                                    'auriventris') ) %>%
          mutate( species = replace(species, 
                                    genus == 'Hadrotetti' & species == 'trifascia',
                                    'trifasciatus') ) %>%
          mutate( species = replace(species, 
                                    genus == 'Mermiria' & species == 'bivitatta',
                                    'bivittata') ) %>%
          mutate( species = replace(species, 
                                    genus == 'Melanoplus' & species == 'bivittatu',
                                    'bivittatus') ) %>%
          mutate( genus = replace(genus, genus == 'Hadrotetti', 'Hadrotettix') ) %>% 
          mutate( genus = replace(genus, genus == 'Ageneotett', 'Ageneotettix') ) %>% 
          mutate( genus = replace(genus, genus == 'Campylacan', 'Campylacantha') ) %>% 
          mutate( genus = replace(genus, genus == 'Campylacan', 'Campylacantha') ) %>% 
          mutate( genus = replace(genus, genus == 'Brachystol', 'Brachystola') ) %>%
          mutate( genus = replace(genus, genus == 'Orphullela', 'Orphulella') ) %>%
          # same code to identify species
          mutate( species = replace(species, species == 'species', 'spp.') ) %>% 
          mutate( species = replace(species, is.na(species) , 'spp.') ) %>% 
  
          # LUMP data to genus
          mutate( species = replace(species, genus == 'Pardalopho', 'spp.') ) %>% 
          mutate( species = replace(species, genus == 'Mermiria', 'spp.') ) %>%         
          mutate( species = replace(species, genus == 'Hesperotet', 'spp.') ) %>%         
          mutate( species = replace(species, genus == 'Arphia', 'spp.') ) %>%
          mutate( species = replace(species, genus == 'Pardalophora', 'spp.') ) %>%
      
          # DROP genus information ()
          subset( !(genus == 'Encoptolop' & species == 'spp.' ) ) %>% 
          # I DROP MELANOPUS BECAUSE IT'S A VERY DIVERSE GENUS
          subset( !(genus == 'Melanopus' & species == 'spp.' ) ) %>% 

          # re-aggregate across new taxonomic units
          mutate( COUNT = as.numeric(COUNT) ) %>% 
          group_by( RECYEAR, RECMONTH, RECDAY, 
                    WATERSHED, REPSITE, SOILTYPE, REPSITE,
                    genus, species) %>% 
          summarise( COUNT = sum(COUNT,na.rm=T) ) %>%  
          mutate( SITE_ID = paste(WATERSHED, REPSITE, SOILTYPE, sep="_") ) %>% 
          group_by( RECYEAR, SITE_ID, genus, species ) %>% 
          summarise( TOTAL = mean(COUNT, na.rm=T) ) %>% 
          ungroup %>% 
  
          # FORMAT
          mutate( VARIABLE_NAME = paste(genus, species, sep='_') ) %>% 
          rename( VALUE         = TOTAL,
                  DATE          = RECYEAR ) %>% 
          mutate( OBSERVATION_TYPE = "TAXON_COUNT",
                  VARIABLE_UNITS = "COUNT (average)") %>% 
          select(DATE, SITE_ID, OBSERVATION_TYPE, VARIABLE_UNITS, VARIABLE_NAME, VALUE) # %>% 
  
# sites to keep
keep_s <- c('0SPB_B_fl', '0SPB_A_fl', '002C_B_fl', '002C_A_fl', 
            '0SUB_B_fl', '004B_A_fl', '004B_B_fl', '001D_B_fl', 
            '001D_A_fl', '004F_B_fl', '004F_A_fl', '002D_B_fl',
            '002D_A_fl')

#select plots and years
out <- knz %>% 
          subset( SITE_ID %in% keep_s ) %>% 
          subset( DATE > 1990)

write.csv(out, 'C:/L3-knz-grasshopper-compagnoni.csv', row.names=F)
