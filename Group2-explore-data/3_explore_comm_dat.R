# ---------------------------------------- #
# Explore community data - 30 Mar 2017     #
# ---------------------------------------- #

# Clear environment
rm(list = ls())

# Make sure your working environment is set to the GitHub repository ltermetacommunities. 
#Check to make sure working directory is correct
if(basename(getwd())!="ltermetacommunities"){cat("Plz change your working directory. It should be 'ltermetacommunities'")}


# Check for and install required packages
#library()

for (package in c('dplyr', 'tidyr', 'vegetarian', 'vegan', 'metacom', 'ggplot2', 'iNEXT', 'grDevices', 'RColorBrewer','BiodiversityR')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# ---------------------------------------------------------------------------------------------------
# Assign data set of interest# Assign L3 data set of interest
# NOTE: Google Drive file ID is different for each dataset

#in tex file
# CSUN-USVI-coral
data.set <- "usvi-coral-castorani"
data.key <- "0BxUZSA1Gn1HZZGowdUVCTTdtXzg" # Google Drive file ID

# mcr-algae-castorani
data.set <- "mcr-algae-castorani"
data.key <- "0BxUZSA1Gn1HZenhxaVJ6bWtVdDg" # Google Drive file ID

# jrn-lizard-hope 
data.set <- "jrn-lizard-hope"
data.key <- "0B7o8j0RLpcxiYW10X1djMTBGM0U" # Google Drive file ID 

# nwt-plants-hallett 
data.set <- "nwt-plants-hallett"
data.key <- "0B2P104M94skvQzE2QUMtNHpCcXc" # Google Drive file ID 

#needs work still
# fce-algae-marazzi (contains duplicated records... double check)
data.set <- "fce-algae-marazzi"
data.key <- "0B2P104M94skvbVdsYUc4amdSLWc" # Google Drive file ID 


#----------------------------------------------------------------------------------------------------
# MAKE DATA LIST
dat <- list()

# IMPORT DATA
L3dat <-  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", data.key), stringsAsFactors=F) 


#Work around if no internet access, here's a workaround assuming the Google Drive directory is stored locally:
#L3dat <- read.csv(paste("~/Google Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/L3-", data.set, ".csv", sep=""), stringsAsFactors=F)


# Subset out community data  and add it to dat list
dat$comm.long <- subset(L3dat, OBSERVATION_TYPE=='TAXON_COUNT')

# Convert community data to wide form
comm.wide <- dat$comm.long %>%
  select(-VARIABLE_UNITS) %>%
  spread(VARIABLE_NAME,  VALUE)

dat$comm.wide <- comm.wide
summary(dat)

#---------------------------------------------------------------------------------------------------
# Create color palette for heatmaps
heat.pal.spectral <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

#---------------------------------------------------------------------------------------------------
# CHECK DATA STRUCTURE AND SPATIOTEMPORAL SAMPLING EFFORT

# How are communities sampled across space and time?
ggplot(data = dat$comm.long, aes(x = DATE, y = SITE_ID)) +
  geom_point(size = 5) +
  theme_bw() +
  xlab("Year") +
  ylab("Site") +
  theme_bw()

# Write spatiotemporal sampling effort plot to pdf 
pdf(file=paste('~/Google Drive/LTER Metacommunities/Manuscripts/MS3_metacom-stability-analysis/supplemental_methods_figs/', data.set, '_spatiotemporal_sampling_effort.pdf',sep=''))
ggplot(data = dat$comm.long, aes(x = DATE, y = SITE_ID)) +
  geom_point(size = 5) +
  theme_bw() +
  xlab("Year") +
  ylab("Site") +
  theme_bw() +
  theme(axis.title = element_text(size=20))
dev.off()


# Check the propagation of species across space and time
tapply(dat$comm.long$VALUE, list(dat$comm.long$SITE_ID,dat$comm.long$DATE), length)

#---------------------------------------------------------------------------------------------------
# ALPHA DIVERSITY (SPECIES RICHNESS) OVER TIME AND SPACE

# Examine temporal patterns in observations of the number of species
no.taxa.fun <- function(ex) {
  
  # Number of unique taxa at each site through time
   no.taxa <- ex %>%
    filter(VALUE > 0) %>%
    select(DATE, VARIABLE_NAME, SITE_ID) %>%
    unique() %>%
    mutate(no.taxa = 1) %>%
    group_by(SITE_ID, DATE) %>%
    summarize(no.taxa = sum(no.taxa))
  
   # Summed number of unique taxa among all sites through time
  total.no.taxa <- ex %>%
    filter(VALUE > 0) %>%
    select(DATE, VARIABLE_NAME) %>%
    unique() %>%
    mutate(no.taxa = 1) %>%
    group_by(DATE) %>%
    summarize(no.taxa = sum(no.taxa))
  
  return(list("no.taxa" = no.taxa, "total.no.taxa" = total.no.taxa))
}

# Apply function across community data
no.taxa <- no.taxa.fun(dat$comm.long) # Result is a list of: (1) no. of taxa at each site; (2) no. of taxa at all sites

# Plot a heatmap of the number of species observed over space and time
ggplot(data = no.taxa$no.taxa, aes(x = DATE, y = SITE_ID, fill = no.taxa)) +
  geom_raster() +
  scale_fill_gradientn(colours = heat.pal.spectral(100), name = "No. of taxa") +
  theme_bw() +
  #guides(fill = guide_legend(title = "Number of taxa")) +
  xlab("Year") +
  ylab("Site") +
  theme(aspect.ratio = 1)


# Plot number of taxa through time
ggplot(data=no.taxa$no.taxa, aes(x=DATE, y=no.taxa)) +
  geom_point(aes(color = SITE_ID)) +
  geom_line(aes(color=SITE_ID)) +
  geom_point(data=no.taxa$total.no.taxa, aes(x=DATE, y=no.taxa), color="black", size=3) +
  geom_line(data=no.taxa$total.no.taxa, aes(x=DATE, y=no.taxa), color="black", size=1) +
  xlab("Year") +
  ylab("Number of taxa observed") +
  guides(color = guide_legend(title = "Site")) +
  ylim(c(0, max(no.taxa$total.no.taxa$no.taxa))) +
  theme_bw()
# Note that the thick line indicates the total number of taxa among all sites

#write to file:
pdf(file=paste('~/Google Drive/LTER Metacommunities/Manuscripts/MS3_metacom-stability-analysis/supplemental_methods_figs/', data.set, '_num_taxa_over_time.pdf',sep=''))
ggplot(data=no.taxa$no.taxa, aes(x=DATE, y=no.taxa)) +
  geom_point(aes(color = SITE_ID)) +
  geom_line(aes(color=SITE_ID)) +
  geom_point(data=no.taxa$total.no.taxa, aes(x=DATE, y=no.taxa), color="black", size=3) +
  geom_line(data=no.taxa$total.no.taxa, aes(x=DATE, y=no.taxa), color="black", size=1) +
  xlab("Year") +
  ylab("Number of taxa observed") +
  guides(color = guide_legend(title = "Site")) +
  ylim(c(0, max(no.taxa$total.no.taxa$no.taxa))) +
  theme_bw() +
  theme(axis.title = element_text(size=20))
  dev.off()
# ---------------------------------------------------------------------------------------------------
# SITE-SPECIFIC AND TOTAL SPECIES ACCUMULATION CURVES

# Make a function that returns the cumulative number of taxa observed for a given set of community data
cuml.taxa.fun <- function(EX){
  taxa.t.list <- list() # Make empty list
  
  # Loop over each year, creating a list that contains the unique taxa found in each year
  for(t in 1:length(unique(EX$DATE))){
    tmp.dat <- subset(EX, EX$DATE == t + (min(EX$DATE) - 1))
    tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0) 
    taxa.t.list[[t]] <- unique(tmp.dat.pres$VARIABLE_NAME)
  }
  
  # Make cumulative list of taxa through time
  cuml.taxa <- list() # Empty list
  cuml.taxa[[1]] <- taxa.t.list[[1]] # Add the taxa from the first time step 
  
  # Run for-loop to create list of the cumulative taxa, with duplicates
  for(t in 2:length(unique(EX$DATE))){ 
    cuml.taxa[[t]] <- c(cuml.taxa[[t - 1]], taxa.t.list[[t]])
  }
  
  # Remove duplicates
  cuml.taxa <- lapply(cuml.taxa, function(x){unique(x)})
  
  # Return the number of total unique taxa through time
  cuml.no.taxa <- data.frame("year" = unique(EX$DATE))
  cuml.no.taxa$no.taxa <- unlist(lapply(cuml.taxa, function(x){length(x)}))
  
  return(cuml.no.taxa)
  }

# Calculate cumulative taxa (i.e., species accumulation) across all sites pooled together
cuml.taxa.all.sites <- cuml.taxa.fun(EX = dat$comm.long)

# Examine site-level patterns of species accumulation
# First, sort the comm.dat dataframe to make sure its ordered by site
comm.dat <- dat$comm.long %>% 
  arrange(SITE_ID)

# Then, split the data frame, and apply the cuml.taxa.fun() for each site
X <- split(comm.dat, comm.dat$SITE_ID)
out <- lapply(X, cuml.taxa.fun)

# Make the lists a dataframe
output <- do.call("rbind", out)

# Extract rownames to create a SITE_ID column
output$rnames <- row.names(output)

# Clean up the SITE_ID column
cuml.taxa.by.site <- output %>%
  tbl_df() %>%
  separate(rnames, c("SITE_ID", "todrop")) %>%
  select(-todrop)

# Plot the cumulative number of taxa observed at each site, as well as across all sites together
ggplot(data=cuml.taxa.by.site, aes(x = year, y = no.taxa)) +
  geom_point(aes(color = SITE_ID)) +
  geom_line(aes(color = SITE_ID)) +
  geom_point(data = cuml.taxa.all.sites, aes(x=year, y=no.taxa), size = 3) +
  geom_line(data = cuml.taxa.all.sites, aes(x=year, y=no.taxa), size = 1.5) +
  xlab("Year") +
  ylab("Cumulative number of taxa") +
  guides(color = guide_legend(title = "Site")) +
  ylim(c(0, max(cuml.taxa.all.sites$no.taxa))) +
  theme_bw()
# Note that the thick line indicates the total number of taxa among all sites

# Write species accumulation curve to pdf
pdf(file=paste('~/Google Drive/LTER Metacommunities/Manuscripts/MS3_metacom-stability-analysis/supplemental_methods_figs/',data.set,'_species_accumulation_curve.pdf',sep=''))
ggplot(data=cuml.taxa.by.site, aes(x = year, y = no.taxa)) +
  geom_point(aes(color = SITE_ID)) +
  geom_line(aes(color = SITE_ID)) +
  geom_point(data = cuml.taxa.all.sites, aes(x=year, y=no.taxa), size = 3) +
  geom_line(data = cuml.taxa.all.sites, aes(x=year, y=no.taxa), size = 1.5) +
  xlab("Year") +
  ylab("Cumulative number of taxa") +
  guides(color = guide_legend(title = "Site")) +
  ylim(c(0, max(cuml.taxa.all.sites$no.taxa))) +
  theme_bw()  +
  theme(axis.title = element_text(size=20))
dev.off()

#make metadata table
mtdt <- list()
mtdt$dataset <- data.set
mtdt$initial.year <- min(dat$comm.wide$DATE)
mtdt$study.length <- max(dat$comm.wide$DATE) -  min(dat$comm.wide$DATE) +1
mtdt$n.years <- length(unique(dat$comm.wide$DATE))
mtdt$n.plots <- length(unique(dat$comm.wide$SITE_ID))
mtdt$n.taxa <- length(unique(dat$comm.long$VARIABLE_NAME))
mtdt$organism <- gsub(".*-(.*)\\-.*", "\\1", data.set)
mtdt <- data.frame(mtdt)
write.csv(mtdt, file = paste("~/Google Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/metadata_tables/",data.set,"_metadata.csv", sep=""), row.names=F)




####################################################
####################################################
# Below here, not included in QA/QC, yet....


# --------------------------------------------------------------------------------------------------
# Row and column summary statistics for comm.wide data to aid in screening for the amount and pattern of missing data

# Function for row and column summary statistics of community data
sum.stats <-
  function(x,var='',by='',margin='column',...){
    
    if(!var==''){
      y<-subset(x,select=eval(parse(text=var))) #select variables to summarize
    }
    else{y<-x}
    
    variable<-colnames(y)
    sample<-rownames(y)
    
    #statistical functions
    nobs<-function(x) length(x)
    cv<-function(x,na.rm) sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE)*100 
    zeros<-function(x,na.rm) sum(x==0,na.rm=TRUE)
    pct.zeros<-function(x,na.rm) sum(x==0,na.rm=TRUE)/length(x)*100
    nobs.missing<-function(x,na.rm) sum(is.na(x))
    pct.missing<-function(x,na.rm) sum(is.na(x))/length(x)*100 
    se<-function(x,na.rm) sd(x,na.rm=TRUE)/sqrt(length(x)-sum(is.na(x)))
    se.ratio<-function(x,na.rm) se(x)/mean(x,na.rm=TRUE)*100
    richness<-function(x,na.rm) nobs(x)-zeros(x)-nobs.missing(x)
    sh.diversity<-function(x,na.rm) -sum(((x)/sum(x,na.rm=TRUE))*log(((x)/sum(x,na.rm=TRUE))),na.rm=TRUE)
    sh.evenness<-function(x,na.rm) sh.diversity(x)/log(richness(x))
    si.diversity<-function(x,na.rm){
      if(richness(x)==0) 0
      else 1-sum(((x)/sum(x,na.rm=TRUE))*((x)/sum(x,na.rm=TRUE)),na.rm=TRUE)
    }
    si.evenness<<-function(x,na.rm) si.diversity(x)/(1-(1/richness(x)))
    
    if(by==''){ #summary table w/o groups
      if(margin=='column'){ #summary table by columns
        z1<-data.frame(apply(y,2,function(x){ #calculate stats
          z1<-c(nobs(x),min(x,na.rm=TRUE),max(x,na.rm=TRUE),
                mean(x,na.rm=TRUE),median(x,na.rm=TRUE),sum(x,na.rm=TRUE),
                sd(x,na.rm=TRUE),cv(x),zeros(x),pct.zeros(x),nobs.missing(x),
                pct.missing(x),se(x),se.ratio(x),richness(x),sh.diversity(x),
                sh.evenness(x),si.diversity(x),si.evenness(x))
          names(z1)<-c('nobs','min','max','mean',
                       'median','sum','sd','cv','zeros','pct.zeros',
                       'nobs.missing','pct.missing','se','se.ratio',
                       'richness','sh.diversity','sh.evenness',
                       'si.diversity','si.evenness') #create col names
          z1<-round(z1,3) #round elements to 3 decimal places
        }))
        z2<-data.frame(t(apply(z1,1,function(x){ #calculate stats on stats
          z2<-c(nobs(x),min(x,na.rm=TRUE),max(x,na.rm=TRUE),
                mean(x,na.rm=TRUE),median(x,na.rm=TRUE),sd(x,na.rm=TRUE),cv(x))
          names(z2)<-c('nobs','min','max','mean',
                       'median','sd','cv') #create row names
          z2<-round(z2,3) #round elements to 3 decimal places
        })))
        z<-list(z1,z2) #create list with col stats and sum stats
        names(z)<-c('Column.Summary','Table.Summary')
      } #end summary table by columns
      
      else{ #summary table by rows
        z1<-data.frame(t(apply(y,1,function(x){ #calculate stats
          z1<-c(nobs(x),min(x,na.rm=TRUE),max(x,na.rm=TRUE),
                mean(x,na.rm=TRUE),median(x,na.rm=TRUE),sum(x,na.rm=TRUE),
                sd(x,na.rm=TRUE),cv(x),zeros(x),pct.zeros(x),nobs.missing(x),
                pct.missing(x),se(x),se.ratio(x),richness(x),sh.diversity(x),
                sh.evenness(x),si.diversity(x),si.evenness(x))
          names(z1)<-c('nobs','min','max','mean',
                       'median','sum','sd','cv','zeros','pct.zeros',
                       'nobs.missing','pct.missing','se','se.ratio',
                       'richness','sh.diversity','sh.evenness',
                       'si.diversity','si.evenness') #create col names
          z1<-round(z1,3) #round elements to 3 decimal places
        })))
        z2<-data.frame(apply(z1,2,function(x){ #calculate stats on stats
          z2<-c(nobs(x),min(x,na.rm=TRUE),max(x,na.rm=TRUE),
                mean(x,na.rm=TRUE),median(x,na.rm=TRUE),sd(x,na.rm=TRUE),cv(x))
          names(z2)<-c('nobs','min','max','mean',
                       'median','sd','cv') #create row names
          z2<-round(z2,3) #round elements to 3 decimal places
        }))
        z<-list(z1,z2) #create list with row stats and sum stats
        names(z)<-c('Row.Summary','Table.Summary')
      } #end summary table by rows
    } #end summary table w/o groups
    
    else{ #summary table w/ groups
      #	write('',file=paste(outfile,'.csv',sep='')) #empty outfile if it exists
      fns<-c('nobs','min','max','mean',
             'median','sum','sd','cv','zeros','pct.zeros',
             'nobs.missing','pct.missing','se','se.ratio',
             'richness','sh.diversity','sh.evenness',
             'si.diversity','si.evenness') #create names vector
      n<-by.names(x,by) #create by variable
      for(i in 1:length(fns)){ #loop thru by groups
        cat(t<-paste(strtrim(paste('--',fns[i],paste(rep('-',80),collapse='')),80),'\n')) #create line break
        q<-list(n[,2]) #create a list of group names
        names(q)<-names(n)[2] #assign by name to q
        z1<-aggregate(y,q,fns[i],na.rm=TRUE) #calculate stats
        zz1<-round(z1[,2:ncol(z1)],3) #round stats to 3 decimal places
        g<-z1[,1,,drop=FALSE] #select group variable
        z1<-cbind(g,zz1) #bind group variable with selected variables
        z2<-data.frame(t(apply(z1[,-1],1,function(x){ #calculate stats on stats
          z2<-c(nobs(x),min(x,na.rm=TRUE),max(x,na.rm=TRUE),
                mean(x,na.rm=TRUE),median(x,na.rm=TRUE),sd(x,na.rm=TRUE),cv(x))
          names(z2)<-c('nobs','min','max','mean',
                       'median','sd','cv') #create row names
          z2<-round(z2,3) #round elements to 3 decimal places
        })))
        z<-cbind(z1,z2) #bind col stats with summary stats
        print(z) #print to console
      } #end loop thru groups
    } #end summary table w/ groups
    return(z)
  }

# Column summary for each species' abundance data
# Note this assumes that the first three columns of the comm.wide data are always: OBSERVATION_TYPE, SITE_ID, DATE
sum.stats(dat$comm.wide[,-c(1:3)])

# Row summaries for species data
sum.stats(dat$comm.wide[,-c(1:3)], margin='row')

# ---------------------------------------------------------------------------------------------------
# RANK ABUNDANCE CURVES

rank.abund.dat <- dat$comm.wide
rank.abund.dat$SITE_ID <- as.factor(rank.abund.dat$SITE_ID)
rank.abund.dat$DATE <- as.factor(rank.abund.dat$DATE)

# This section uses code from the BiodiversityR package
# Create rank abundance curve for all sites over all dates
ra_allsitestimes <- rankabundance(rank.abund.dat[,-c(1:3)])
rankabunplot(ra_allsitestimes)

# Plot proportional rank abundance curve for each site.
# legend is set to false, but set it to true to see legend for each site
rankabuncomp(x=rank.abund.dat[,-c(1:3)], y=rank.abund.dat[,c(1:3)], factor="SITE_ID", scale='proportion', legend=FALSE)

# Plot proportional rank abundance curve for each date.
# legend is set to false, but set it to true to see legend for each site
rankabuncomp(x=rank.abund.dat[,-c(1:3)], y=rank.abund.dat[,c(1:3)], factor="DATE", scale='proportion', legend=FALSE)

# ---------------------------------------------------------------------------------------------------
# RAREFACTION CURVES
# Note this code will only run for data where the community data are whole number counts!!


# This section uses code from the iNEXT package
# Convert dat$comm.wide from a list to a matrix for the iNEXT function
comm_wide_mat <- matrix(unlist(dat$comm.wide[,-c(1:3)]), ncol = dim(dat$comm.wide)[2]-3, byrow = TRUE)

# Use iNEXT function to interpolate and extrapolate Hill numbers for rarefaction curves of all sites together
# Note this step takes a while to run.
rarefaction_all <- iNEXT(comm_wide_mat, q=c(0,1,2), datatype="abundance")
# Plot rarefaction curves for all sites considered together
ggiNEXT(rarefaction_all, se=TRUE, color.var="order")

# Loop through each site separately and generate rarefaction curve
for(i in 1:length(dat$comm.wide$SITE_ID)){
  # Convert dat$comm.wide from a list to a matrix for the iNEXT function for each site
  comm_wide_site <- dat$comm.wide[,SITE_ID==names(dat$comm.wide$SITE_ID[i])]
  comm_wide_sitemat <- matrix(unlist(dat$comm.widesite[,-c(1:3)]), ncol = dim(dat$comm.wide)[2]-3, byrow = TRUE)
  
  # Use iNEXT function to interpolate and extrapolate Hill numbers for rarefaction curves of site i
  rarefaction_site <- iNEXT(comm_wide_sitemat, q=c(0,1,2), datatype="incidence_freq")
  # Plot rarefaction curves for site i
  ggiNEXT(rarefaction_site, se=TRUE, color.var="order")
}

# write rarefaction curves for all sites considered together to pdf
pdf(file=paste('Group2-explore-data/rarefaction_curves/', data.set, '_rarefaction_curves.pdf',sep=''))
ggiNEXT(rarefaction_all, se=TRUE, color.var="order")
dev.off()