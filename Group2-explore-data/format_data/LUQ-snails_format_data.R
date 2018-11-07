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

# check how many sites we have every single year
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
                summarise( count = mean(count, na.rm=T) ) %>% 

                # LTERMETACOMM FORMAT
                rename( SITE_ID       = Point,
                        VARIABLE_NAME = spp_code,
                        DATE          = year,
                        VALYE         = count ) %>% 
                mutate( OBSERVATION_TYPE = 'TAXON_COUNT',
                        VARIABLE_UNITS   = 'COUNT (yearly mean)' ) %>% 
                # order data
                select(OBSERVATION_TYPE, SITE_ID, DATE, 
                       VARIABLE_NAME, VARIABLE_UNITS, VALUE)

# write file out
write.csv(snail_long, 'C:/L3-luq-snails-compagnoni.csv', row.names=F)

