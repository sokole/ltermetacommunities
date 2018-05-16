# Aldo Compagnoni 5.15.2018
# formatting the SEVILLETTA grasshopper dataset

infile1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sev/106/214968/1def46af7a0b7d367705caaf2697f0d2" 
infile1 <- sub("^https","http",infile1)

dt1     <-read.csv(infile1,header=F ,skip=1,sep="," ,quot='"' 
                    , col.names=c(
                    "Date",     
                    "PER",     
                    "Site",     
                    "Web",     
                    "Transect",     
                    "Species",     
                    "AGE",     
                    "gender",     
                    "substrate",     
                    "Count",     
                    "BURNED",     
                    "Comments"), check.names=TRUE) 

# format 
form_d <- dt1 %>% 
            separate(col = 'Date', into = c('month','day','year') ) %>% 
            # sum counts over years, sex, age ground, and substrate
            group_by(year,Site,Web,Transect, Species) %>% 
            summarise( year_count = sum(Count) ) %>% 
            # create identifier for site/Web/Transect combination
            mutate( spatial_rep = paste(Site,Web,Transect, sep = '_') ) %>% 
            ungroup %>% 
            # select and format only relevant data
            select(year,spatial_rep, Species, year_count) %>% 
            rename(VALUE   = year_count,
                   SITE_ID = spatial_rep,
                   VARIABLE_NAME = Species,
                   DATE = year) %>% 
            mutate(OBSERVATION_TYPE = "TAXON_ABUNDANCE",
                   VARIABLE_UNITS = "Count (two censuses a year)" ) 

# species codes (not needed)
spp_d <- read.csv('C:/cloud/Dropbox/database-development (1)/data/sev-data/sev106_spp_codes.csv',
                  sep=',', header=T)


site_d <- data.frame(OBSERVATION_TYPE=rep("SPATIAL_COORDINATE",8),
                     SITE_ID=c(),
                        DATE=rep(NA,4),
                        VARIABLE_NAME=c(rep("latitude",4),rep("longitude",4)),
                        VARIABLE_UNITS=rep("dec. degrees",8),
                        VALUE=c(rep(45.4038890,4),rep(-93.2680560,4))
                    )

# lat/lon  Five Points Black Grama (BOER)
-106.736 	
34.3331

# Creosotebush site (LATR)
-106.7358 	
34.3331

# Pinyon-Juniper (PJ)
-106.535 
34.368

# The Blue Grama (BOGR)
-106.631 
34.3348

# done (for now)