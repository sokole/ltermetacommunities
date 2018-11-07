#Aldo Compagnoni June 2018
#NKL added data provenance
rm(list = ls(all = T))

options(stringsAsFactors = F)
library(dplyr)
library(tidyr)
library(stringi)


# Package ID: knb-lter-knz.29.12 Cataloging System:https://pasta.edirepository.org.
# Data set title: CGR02 Sweep Sampling of Grasshoppers on Konza Prairie LTER watersheds.
# Data set creator:  Anthony Joern -  
# Metadata Provider:    - Konza LTER 
# Contact:  Konza LTER -    - knzlter@ksu.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

#environmental data
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/29/12/aaa7b3b477019f6bb96f47a9a6ae8943" 
infile1 <- sub("^https","http",infile1) 
 dt1 <-read.csv(infile1, stringsAsFactors=F)

#counts 
infile2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/29/12/3fb352e2478f776517f7e880fe31b808" 
infile2 <- sub("^https","http",infile2) 
 dt2 <-read.csv(infile2, stringsAsFactors=F)
 
site_df <- dt1
count_df <- dt2


# Alternately, read raw data from copy cached on Google Drive by Nina Lany Nov 7, 2018: information on sites, and information on species counts
#site_df   <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/KNZ-grasshoppers/archive_knb-lter-knz/CGR021.csv") 
#count_df  <- read.csv("~/Google Drive FIle Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/KNZ-grasshoppers/archive_knb-lter-knz/CGR022.csv")

names(count_df) <- toupper(names(count_df))
names(site_df) <- toupper(names(site_df))

# remove DATACODE, RECTYPE: they will impede a "join". Plus, they are undefined in metadata
site_df  <- site_df %>% 
              setNames( toupper(names(.)) ) %>% 
              select(-DATACODE,-RECTYPE)

count_df <- count_df %>% 
              setNames( toupper(names(.)) ) %>% 
              select(-DATACODE,-RECTYPE)

# check "SPCODE" versus taxonomic information
knz   <- left_join(count_df, site_df) %>% 
            # format the total number of counts per sweep
            subset( TOTAL != '1 01') %>% 
            mutate( TOTAL = replace(TOTAL, TOTAL == "", NA) ) %>% 
            mutate( TOTAL = as.numeric(TOTAL) ) %>% 
            # remove SPCODES ( could create problems with "doubles")
            select(-SPCODE) %>% 
            # remove sweeps ( summarized in "TOTAL")
            select(-c(S1:S10)) %>% 
    
            # change clear mistakes
            mutate( SPECIES = replace(SPECIES, SPECIES == 'brachystol magna',
                                      'Brachystola magna') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'schistocer lineata',
                                      'Schistocerca lineata') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'paratylotr brunneri',
                                      'paratylota brunneri') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'hypochlora alba',
                                      'Hypochlora alba') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'campylacan olivacea',
                                      'Campylacantha olivacea') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'hesperotet speciosus',
                                      'Hesperotettix speciosus') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'hesperotet viridis',
                                      'Hesperotettix viridis') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'hesperotet species',
                                      'hesperotet spp.') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'phoetaliot nebrascen',
                                      'Phoetaliotes nebrascensis') ) %>% 
            mutate( SPECIES = gsub('melanoplus','Melanoplus',SPECIES) ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'Melanoplus femurrubr',
                                      'Melanoplus femurrubrum') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'Melanoplus bivittatu',
                                      'Melanoplus bivittatus') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'Melanoplus species',
                                      'Melanoplus spp.') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'syrbula admirabil',
                                      'Syrbula admirabilis') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'orphulella speciosa' | 
                                               SPECIES == 'orphullela speciosa' ,
                                      'Orphulella speciosa') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'mermiria picta',
                                      'Mermiria picta') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'mermiria bivitatta',
                                      'mermiria bivittata') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'boopedon auriventr',
                                      'Boopedon auriventris') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'boopedon gracile',
                                      'Boopedon gracile') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'ageneotett deorum',
                                      'Ageneotettix deorum') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'mermiria species' | 
                                               SPECIES == 'mermiria spp.' ,
                                      'Mermiria spp.') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'hadrotetti trifascia',
                                              'Hadrotettix trifasciatus') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'pardalopho haldemani',
                                            'Pardalophora haldemani') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'arphia species' | 
                                               SPECIES == 'arphia spp.' ,
                                      'Arphia spp.') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'unknown' | 
                                               SPECIES == 'unknown ' ,
                                      'Unknown') ) %>% 
            mutate( SPECIES = replace(SPECIES, SPECIES == 'pardalopho spp.',
                                              'Pardalophora spp.') ) %>% 
            
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
    
            # FIX mistakes in genus/species names
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
        
            # DROP genus information when need be
            subset( !(genus == 'Encoptolop' & species == 'spp.' ) ) %>% 
            # I DROP MELANOPUS SPP. BECAUSE IT'S A VERY DIVERSE GENUS
            subset( !(genus == 'Melanopus' & species == 'spp.' ) ) %>% 
    
            # REMOVE DOUBLES
            unique %>% 
            
            # retain all potential groups - except the sweeps
            group_by( RECYEAR, RECMONTH, RECDAY, 
                      WATERSHED, REPSITE, SOILTYPE,
                      genus, species) %>% 
            # SUM across Totals: this takes care of "lumped" taxonomies
            summarise( TOTAL = sum(TOTAL,na.rm=T) ) %>%  
              
            # create SITE_ID column
            mutate( SITE_ID = paste(WATERSHED, REPSITE, SOILTYPE, sep="_") ) %>% 
  
            # average counts across year (variable sampling frequency)
            group_by( RECYEAR, SITE_ID, genus, species ) %>% 
            summarise( TOTAL = mean(TOTAL, na.rm=T) ) %>% 
            ungroup %>% 
    
            # FORMAT using metacommunity standards
            mutate( VARIABLE_NAME = paste(genus, species, sep='_') ) %>% 
            rename( VALUE         = TOTAL,
                    DATE          = RECYEAR ) %>% 
            mutate( OBSERVATION_TYPE = "TAXON_COUNT",
                    VARIABLE_UNITS = "COUNT (average)") %>% 
            select(DATE, SITE_ID, OBSERVATION_TYPE, VARIABLE_UNITS, VARIABLE_NAME, VALUE) 
  

# sites to keep
keep_s <- c('0SPB_B_fl', '0SPB_A_fl', '002C_B_fl', '002C_A_fl', 
            '0SUB_B_fl', '004B_A_fl', '004B_B_fl', '001D_B_fl', 
            '001D_A_fl', '004F_B_fl', '004F_A_fl', '002D_B_fl',
            '002D_A_fl') %>% tolower

#select plots and years
out <- knz %>% 
          subset( SITE_ID %in% keep_s ) %>% 
          subset( DATE > 1995)

#propogate zeros and check 
out <- out %>% spread(VARIABLE_NAME, VALUE, fill = 0) %>% 
  gather(VARIABLE_NAME, VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS)

tapply(out$VALUE, list(out$SITE_ID, out$DATE), length)


write.csv(out, '~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-knz-grasshopper-compagnoni.csv', row.names=F)
