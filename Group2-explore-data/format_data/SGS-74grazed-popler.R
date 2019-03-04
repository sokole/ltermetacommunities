# --------------------------------------------------------- #
# Format raw data from L0 into L3 format                    #
# handy code TEMPLATE                                       #
# Revised 15 May 2018 by ERS                                #
# --------------------------------------------------------- #

options(stringsAsFactors = FALSE)

# Clear environment
rm(list = ls())


#Check to make sure working directory is set to the ltermetacommunities github
if(basename(getwd())!="ltermetacommunities"){cat("Plz change your working directory. It should be 'ltermetacommunities'")}

# Check for and install required packages
for (package in c('googledrive','dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom', 'ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# read data from EDI portal
infile1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sgs/527/1/c3dc7ed391a6f6c6eca4512d27d0d091" 
infile1 <- sub("^https","http",infile1) 
dt1     <-read.csv(infile1,header=F 
          ,skip=1
            ,sep="\t"  
        , col.names=c(
                    "Year",     
                    "Site",     
                    "Treatment",     
                    "Plot",     
                    "Species",     
                    "Basal_Cover"    ), check.names=TRUE)
               
# format based on ltermetacomm data format
sgs   <- dt1 %>% 
            # only retain sites that WERE ALWAYS GRAZED
            subset( Treatment == 'GZGZ' ) %>% 
            # data gap before 1995
            subset( Year > 1994 ) %>% 
            # aggregated by treatment
            group_by( Year, Site, Species ) %>% 
            summarise( Basal_Cover = mean(Basal_Cover) ) %>% 
            ungroup() %>% 
            rename(SITE_ID = Site, 
                   DATE = Year,
                   VARIABLE_NAME = Species,
                   VALUE = Basal_Cover) %>%
            mutate(VARIABLE_UNITS = 'PERCENT_COVER',
                   OBSERVATION_TYPE = 'TAXON_COUNT') %>%
            select(OBSERVATION_TYPE,SITE_ID, DATE, VARIABLE_NAME, VARIABLE_UNITS, VALUE) %>% 
            # insert zeros
            spread(key = VARIABLE_NAME, value = VALUE, fill = 0) %>% 
            gather(key = VARIABLE_NAME, value = VALUE, -DATE, -SITE_ID, -OBSERVATION_TYPE, -VARIABLE_UNITS) 
          
# check taxonomy using popler's information
library(popler)

# get the sppcodes where genus/species == NA
sgs_pplr  <- pplr_get_data( proj_metadata_key == 74 )
na_taxa   <- sgs_pplr %>% 
                as.data.frame() %>% 
                dplyr::select(sppcode, genus, species) %>% 
                subset( species =='NA' | genus == 'NA') %>% 
                .$sppcode %>% 
                unique()

# remove na_taxa
sgs_out <- subset(sgs, !(VARIABLE_NAME %in% na_taxa) ) 

# write it down 
write.csv(sgs_out, 
          'C:/L3-sgs-plants-compagnoni.csv',
          row.names=F)
