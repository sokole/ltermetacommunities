# --------------------------------------------------------- #
# Format raw data from L0 into L3 format                    #
# handy code TEMPLATE                                       #
# Revised 06 Nov 2018 by CPC                                #
# --------------------------------------------------------- #


# Contributors: Riley Andrade, Max Castorani, Nina Lany, Sydne Record, Nicole Voelker
# Revised by Chris Catano (2018/11/06)

 
options(stringsAsFactors = FALSE)

# Clear environment
rm(list = ls())


# Check for and install required packages
for (package in c('googledrive','dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom', 'ggplot2')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


# Package ID: knb-lter-sgs.140.17 Cataloging System:https://pasta.lternet.edu.
# Data set title: SGS-LTER Long-Term Monitoring Project: Vegetation Cover on Small Mammal Trapping Webs on the Central Plains Experimental Range, Nunn, Colorado, USA 1999 -2006, ARS Study Number 118.
# Data set creator:  Paul Stapp - Department of Biological Science 
# Metadata Provider:    - Colorado State University 
# Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
# Contact:  Paul Stapp -  Department of Biological Science  - pstapp@fullerton.edu
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-sgs.140.17
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

# Import data directly from lter
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sgs/140/17/ef7c6e3224d16d7e0f6335274c16a474" 
infile1 <- sub("^https","http",infile1) 
dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep="\t"  
               , col.names=c(
                 "YEAR",     
                 "WEB",     
                 "KIND",     
                 "DATE",     
                 "RECORDER",     
                 "TRANS",     
                 "PT",     
                 "SPECIES",     
                 "PCT.COVER",     
                 "Comments"    ), check.names=TRUE, stringsAsFactors = F)


#Alternately, read in from Google Drive
#dt1 <- read.csv("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L0-raw/SGS-plants/archive_knb-lter-sgs/LTMntrVegWebsCov.txt", stringsAsFactors=F,sep="\t")
               

# COMMUNITY DATA
# sample design: 3 trapping WEBs in grassland and shrubland (KIND), 3 transects (TRANS)
# per WEB/TYPE, 10 sampling points (PT) per transect 
comm.long <- dt1[, c("YEAR", "WEB", "KIND", "TRANS", "PT", "SPECIES", "PCT.COVER")]

str(comm.long)  # Inspect the structure of the community data
length(unique(comm.long$YEAR)) # 9 years (1999 - 2007)
length(unique(comm.long$SPECIES)) # 147 species (many species codes are inconsistant, must be reconciled)
length(unique(comm.long$WEB)) # 7 WEBS (only 6 mentioned in meta data)

# Combine WEB, KIND, TRANSECT, PT for unique samples
#comm.long$rep <- paste(comm.long$WEB, "_", comm.long$KIND, "_", comm.long$TRANS, "_", comm.long$PT)
#length(unique(comm.long$rep)) #210 unique samples

# Combine WEB, KIND, TRANSECT for unique transects
#comm.long$rep2 <- paste(comm.long$WEB, "_", comm.long$KIND, "_", comm.long$TRANS)
#length(unique(comm.long$rep2)) #21 unique transects

# CONSIDERATION: Should transect or point be level of replication (points)

# code commented out bleow was used to check taxa id mismatches.
# Make vector of unique species
#spec_list <- unique(comm.long$SPECIES)

# Import species list from https://mountainscholar.org/handle/10217/80451 to compare for
# species ID discrepancies
#official_sp_list <- read.csv("C:/Users/Chris/Desktop/Research/metacommunity_group/sgs_species_list_plants.csv")

# Compare official_sp_list with species IDs in comm.long to find mismatches
# First make all codes upper case to avoid case sensitive issues
#official_sp_list$spe_code_upper <- toupper(official_sp_list$spe_code)
comm.long$SPECIES_upper <- toupper(comm.long$SPECIES)
length(unique(comm.long$SPECIES_upper)) # 118 species (many species codes are inconsistant, must be reconciled)

#sp_check <- official_sp_list$spe_code_upper

#spec_list2 <- toupper(spec_list)
#mismatch <- spec_list2[!(spec_list2 %in% sp_check)]
#match <- spec_list2[spec_list2 %in% sp_check]

# output file with all mismatches
# write.csv(mismatch, file = "C:/Users/Chris/Desktop/Research/metacommunity_group/sgs_spec_mismatch.csv")
# see "sgs_spec_mismatch_check.csv" for details of species code changes

comm.long$new_spe <- comm.long$SPECIES_upper #create a new species ID column
df <- comm.long
df <- within(df, new_spe[SPECIES_upper == "KOCHIA"] <- "KOSC")
df <- within(df, new_spe[SPECIES_upper == "C. VILLOSA"] <- "CHVI")
df <- within(df, new_spe[SPECIES_upper == "C.VILLOSA"] <- "CHVI")
df <- within(df, new_spe[SPECIES_upper == "CVILLOSA"] <- "CHVI")
df <- within(df, new_spe[SPECIES_upper == " CAHE"] <- "CAHE")

# create vector of SPECIES codes to drop (because they are not plants, unknown, of have otherwise been corrected)
drop <- c("UNKN FORB", "UNKL", "KOCHIA", "HELIATHUS", "KOSIA", "C. VILLOSA", "UNFB",
          "LARKSPUR", "AL", "CAREX", "COW PAT", "UNK1", "GA", "UNK", "UNK2", "ST", 
          "COW PIE", "C.VILLOSA", "UNKN", "BARE", "C. VILLOSA", "LITTER",
          "CVILLOSA", "DUNG", "CO", "C. VILLOSA", "BARE", "LITT", "OECSP",
          "SP", "SPC", "COWPIE", "UNKNOWN", "BARE ")

dropped <- df[df$new_spe %in% drop, ] # records dropped
df2 <- df[!(df$new_spe %in% drop), ] # records kept

length(unique(df2$new_spe)) # 86 species


# checking for weird taxa names and removing suspicious ones that are rare
comm.long_gamma_summary <- df2 %>% group_by(new_spe) %>% 
  filter(!is.na(new_spe) & PCT.COVER > 0) %>%
  summarize(mean_value = mean(PCT.COVER, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(RA = mean_value / sum(mean_value))
         

dat <- df2[ , c("YEAR", "WEB", "KIND", "TRANS", "PT", "PCT.COVER", "new_spe")]
dat <- dat %>%
  rename(SPECIES = new_spe)

# remove WEB 31W because it was only sampled in 2006 and 2007
dat <- dat[dat$WEB != "31W", ]


# Ensure that community data VALUE and DATE are coded as numeric
# if more than one date per year, aggregate by taking mean counts/biomass/cover across bouts per year.
comm_sampling_summary_table <- dat %>%
  group_by(WEB, YEAR) %>%
  summarize(n_bouts_year = length(unique(YEAR))) 

if(length(unique(comm_sampling_summary_table$n_bouts_year)) > 1){
  message('WARNING: different number of sampling bouts in each different years!')
  print(comm_sampling_summary_table)
}

comm.long2 <- dat %>%   
  mutate_at(vars(c(YEAR, PCT.COVER)), as.numeric) %>%
  group_by(WEB, YEAR, SPECIES) %>%
  summarize(VALUE = mean(PCT.COVER, na.rm = TRUE),
            n_bouts = length(YEAR)) %>%
  select(-n_bouts) %>%
  ungroup()


# Ensure that community character columns coded as factors are re-coded as characters
comm.long2 <- comm.long2 %>%   # Recode if necessary
  mutate_if(is.factor, as.character)
  
# Ensure that SITE_ID is a character: recode numeric as character 
comm.long2 <- comm.long2 %>%   # Recode if necessary
  mutate_at(vars(WEB), as.character)

comm.long3 <- comm.long2 %>%
  rename(DATE = YEAR) %>%
  rename(SITE_ID = WEB) %>%
  rename(VARIABLE_NAME = SPECIES)

comm.long3$VARIABLE_UNITS <- "PCT_COVER"
#comm.long3$TAXON_GROUP <- "PLANTS"

# Double-check that all columns are coded properly
ifelse(FALSE %in% 
   c(
     #class(comm.long$OBSERVATION_TYPE) == "character",
     class(comm.long3$SITE_ID) == "character",
     class(comm.long3$DATE) == "numeric",
     class(comm.long3$VARIABLE_NAME) == "character",
     class(comm.long3$VARIABLE_UNITS) == "character",
     class(comm.long3$VALUE) == "numeric"
     #class(comm.long3$TAXON_GROUP) == "character"),
   ),
  "ERROR: Community columns incorrectly coded.", 
  "OK: Community columns correctly coded.")


# ---------------------------------------------------------------------------------------------------
# Check balanced sampling of species across space and time by inspecting table, and add to data list

comm.long3 <- comm.long3[comm.long3$DATE != 2007,]

# make wide, fill with 0, then back to long to propagate zeros
if(length(unique(xtabs(~ SITE_ID + DATE, data = comm.long3))) > 1){
  comm.long4 <- comm.long3 %>%
    spread(VARIABLE_NAME, VALUE, fill = 0) %>%
    gather('VARIABLE_NAME','VALUE', -c(SITE_ID, DATE, VARIABLE_UNITS))
}

xtabs(~ SITE_ID + DATE, data = comm.long4) #number of taxa should be same in all
hist(na.omit(comm.long4$DATE)) #frequency should be same (even distribution)


ifelse(length(unique(xtabs(~ SITE_ID + DATE, data = comm.long3))) == 1,
       "OK: Equal number of taxa recorded across space and time.", 
       "ERROR: Unequal numbers of observations across space and time, or taxa list not fully propagated across space and time. Inspect contingency table.")

# 2007 not sampled in many sites; remove this year

length(unique(comm.long4$SITE_ID)) # 6 sites 
unique(comm.long4$DATE) # 8 years

# ---------------------------------------------------------------------------------------------------
# Add to dat list the unique taxa
#dat$comm.long <- comm.long

# Convert community data to wide form
comm.wide <- comm.long4 %>%
  select(-VARIABLE_UNITS) %>%
  spread(VARIABLE_NAME,  VALUE)

# check to make sure each species is sampled at least once
colSums(comm.wide[, c(3:87)])

comm.long4 <- comm.long4[comm.long4$VARIABLE_NAME != "ATCO", ]

# Convert community data to wide form
comm.wide <- comm.long4 %>%
  select(-VARIABLE_UNITS) %>%
  spread(VARIABLE_NAME,  VALUE)

# long form data
dat.long <- comm.long4

#output long file (dat.long) to google drive
dat.long$OBSERVATION_TYPE <- rep("TAXON_COUNT", length(dat.long$DATE))


#output long file (dat.long) to google drive

write.csv(dat.long, '~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-sgs-plants-catano.csv', row.names=F)



