#Aldo Compagnoni June 2018
#NKL added data provenance
rm(list = ls(all = T))
options(stringsAsFactors = F)
library(dplyr)
library(tidyr)
library(stringi)
library(popler)

# https://doi.org/10.6073/pasta/ec88f3dd4ed8e172802b52ff3bb82aa8


# Package ID: knb-lter-luq.107.9996736  
# Data set title: El Verde Grid long-term invertebrate data
# Data set creator:  Willig, Michael R.
# Metadata Provider:    - Luquillo LTER 

# read in all of snail data
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-luq/107/9996736/2f2893e94c3687cd0c71ecb2845bb44e" 
infile1   <- sub("^https","http",infile1) 
snail     <-read.csv(infile1,
                     header=F,
                     skip=1,
                     sep=",", 
                     col.names=c(
                        "Year",     
                        "Season",     
                        "Run.ID",     
                        "Point",     
                        "Date",     
                        "Population.size.of.captured.Austroselenites.alticola",     
                        "Population.size.of.captured.Alcadia.alta",     
                        "Population.size.of.captured.Alcadia.striata",     
                        "Population.size.of.captured.Caracolus.caracolla",     
                        "Population.size.of.captured.Caracolus.marginella",     
                        "Population.size.of.captured.Cepolis.musicola",     
                        "Population.size.of.captured.Cepolis.squamosa",     
                        "Population.size.of.captured.Gaeotis.nigrolineata",     
                        "Population.size.of.captured.Lamellaxis.gracilis",     
                        "Population.size.of.captured.Megalomastoma.croceum",     
                        "Population.size.of.captured.Nenia.tridens",     
                        "Population.size.of.captured.Oleacina.glabra",     
                        "Population.size.of.captured.Oleacina.playa",     
                        "Population.size.of.captured.Obeliscus.terebraster",     
                        "Population.size.of.captured.Polydontes.acutangul",     
                        "Population.size.of.captured.Platysuccinea.portoricensis",     
                        "Population.size.of.captured.Subulina.octana",     
                        "Population.size.of.captured.Vaginulus.occidentalis",     
                        "Population.size.of.snails.of.unknown.species",     
                        "Total.snail.abundance",     
                        "Comments.at.the.field"    ), check.names=TRUE)


snail %>% 
  select(Year, Point ) %>% 
  unique %>% 
  count( Year) %>% 
  as.data.frame

# final format for snail data 
snail_long <- snail %>% 
                # put all in long form
                gather(spp_code, 
                       count, 
                       Population.size.of.captured.Austroselenites.alticola:Population.size.of.snails.of.unknown.species) %>%
                # remove data point that lack exact date: popler cannot accomodate this data
                subset(Date != "") %>% 
                # format species names
                mutate( spp_code = gsub('Population.size.of.captured.',
                                        '',
                                        spp_code) ) %>% 
                mutate( spp_code = gsub('\\.','_',spp_code ) ) %>% 
                dplyr::select( -Total.snail.abundance, -Comments.at.the.field ) %>% 
                # separate date column
                separate( Date, c('month','day','year'), sep='/' ) %>% 
                # get count averages
                group_by( year, spp_code, Point) %>% 
                summarise( count = mean(count, na.rm=T) )

# read data directly from the portal (no idea why this is not in popler.....)
cdr_raw <- read.csv('https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-cdr.106.8&entityid=3405c2e271929b0c537492a9ddde102b',
                sep = '\t') 

# clean up dataset
cdr     <- cdr_raw %>% 
            # create site information
            mutate( SITE_ID = as.character(Field.num),
                    yr_mon  = paste(Year,Month,sep='_') ) %>% 
            # select only sites with continuous data
            subset( !(SITE_ID == '28' | SITE_ID == '11') ) %>% 
            # aggregate by species/month
            group_by( Year, SITE_ID, Order, Family, Genus, Specific.epithet) %>% 
            # sum across all months in a year. 
            # Sampling mostly consistent, excet for 2003, when June and August samples were lost in SOME fields
            summarise( count = sum(X.Specimens, na.rm=T) ) %>% 
            ungroup %>% 
            rename( genus   = Genus,
                    species = Specific.epithet ) %>% 
            mutate( species = replace(species, species == 'undet', 'spp.')) %>% 
  
            # Fix taxonomic information
            # species to "Lump" to genus (too high proportion of IDs at the genus level only)
            mutate( species = replace(species, genus == 'Conocephalus', 'spp.'),
                    species = replace(species, genus == 'Scudderia', 'spp.'),
                    species = replace(species, genus == 'Tetrix', 'spp.') ) %>% 
            # remove genus-level IDs for Melanopus
            subset( !(genus == 'Melanoplus' & species == 'spp.') ) %>% 
            select( -Order,-Family ) %>% 
  
            # create ltermetacomm format
            mutate( OBSERVATION_TYPE = 'TAXON_COUNT',
                    VARIABLE_NAME    = paste(genus, species, sep = '_'), 
                    VARIABLE_UNITS   = 'COUNT (monthly sum)' ) %>% 
            rename( DATE             = Year, 
                    VALUE            = count ) %>% 
            select(OBSERVATION_TYPE,SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE)
 
  
# Total sums for taxa, to check % of taxa info at the genus level
taxa <- cdr_raw %>% 
            group_by( Year, Month, Field.num, Order, Family, Genus, Specific.epithet) %>% 
            summarise( count = sum(X.Specimens, na.rm=T) ) %>% 
            ungroup %>% 
            rename( genus   = Genus,
                    species = Specific.epithet) %>% 
            mutate( species = replace(species, species == 'undet', 'spp.')) %>% 
            group_by(genus, species) %>%
            summarise(total = sum(count) ) %>% 
            as.data.frame %>% 
            arrange(genus, species)
          
# write file out
write.csv(cdr, '~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-cdr-grasshopper-compagnoni.csv', row.names=F)

