 #MS1 Figure 1: species accumulation curve (richness and time to asymptote) depends on number of sites sampled.
#NKL 06/27/2018


# Check for and install required packages
for (package in c('dplyr', 'tidyverse', 'vegan', 'googledrive')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


# ---------------------------------------------------------------------------------------------------
# SPECIES ACCUMULATION CURVE FUNCTION - OVER TIME

# Make a function that returns the cumulative number of taxa observed for a given set of community data
cuml.taxa.fun <- function(EX){
  taxa.t.list <- list() # Make empty list
  dates <- unique(EX$DATE)
  # Loop over each year, creating a list that contains the unique taxa found in each year
  for(t in 1:length(unique(EX$DATE))){
    tmp.dat <- subset(EX, EX$DATE == dates[t])
    tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0) 
    taxa.t.list[[t]] <- unique(tmp.dat.pres$VARIABLE_NAME)
  }
  
    # Loop over each year, creating a list that contains the unique taxa found in each year
#  for(t in 1:length(unique(EX$DATE))){
#    tmp.dat <- subset(EX, EX$DATE == t + (min(EX$DATE) - 1))
#    tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0) 
#    taxa.t.list[[t]] <- unique(tmp.dat.pres$VARIABLE_NAME)
#  }
  
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


#---------------------------------------------------------------------------------------------------
# SPECIES ACCUMULATION CURVE FUNCTION - OVER SPACE

# Make a function that returns the cumulative number of taxa observed for a given set of community data
cuml.taxa.space.fun <- function(EX){
  taxa.s.list <- list() # Make empty list
  sites <- unique(EX$SITE_ID)
  # Loop over each year, creating a list that contains the unique taxa found in each year
  for(t in 1:length(unique(EX$SITE_ID))){
    tmp.dat <- subset(EX, EX$SITE_ID == sites[t])
    tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0) 
    taxa.s.list[[t]] <- unique(tmp.dat.pres$VARIABLE_NAME)
  }

  # Make cumulative list of taxa over space
  cuml.taxa.space <- list() # Empty list
  cuml.taxa.space[[1]] <- taxa.s.list[[1]] # Add the taxa from the first sites 
  
  # Run for-loop to create list of the cumulative taxa, with duplicates
  for(t in 2:length(unique(EX$SITE_ID))){ 
    cuml.taxa.space[[t]] <- c(cuml.taxa.space[[t - 1]], taxa.s.list[[t]])
  }
  
  # Remove duplicates
  cuml.taxa.space <- lapply(cuml.taxa.space, function(x){unique(x)})
  
  # Return the number of total unique taxa over space
  cuml.no.taxa.space <- data.frame("site" = unique(EX$SITE_ID))
  cuml.no.taxa.space$no.taxa <- unlist(lapply(cuml.taxa.space, function(x){length(x)}))
  
  return(cuml.no.taxa.space)
  }
 
# PANEL A: cap-birds-banville
L3dat <- read.csv("manuscripts/ms1/L3-cap-birds-banville.csv", header = T, stringsAsFactors=F)

# MAKE DATA LIST
dat <- list()

# Subset out community data and add it to dat list
dat$comm.long <- subset(L3dat, OBSERVATION_TYPE=='TAXON_COUNT')

# Ensure that community data VALUE and DATE are coded as numeric
dat$comm.long <- dat$comm.long %>%   # Recode if necessary
  mutate_at(vars(c(DATE, VALUE)), as.numeric)

  # Ensure that SITE_ID is a character: recode numeric as character 
dat$comm.long <- dat$comm.long %>%   # Recode if necessary
  mutate_at(vars(SITE_ID), as.character)

# Double-check that all columns are coded properly
ifelse(FALSE %in% 
   c(
     class(dat$comm.long$OBSERVATION_TYPE) == "character",
     class(dat$comm.long$SITE_ID) == "character",
     class(dat$comm.long$DATE) == "numeric",
     class(dat$comm.long$VARIABLE_NAME) == "character",
     class(dat$comm.long$VARIABLE_UNITS) == "character",
     class(dat$comm.long$VALUE) == "numeric"
   ),
  "ERROR: Community columns incorrectly coded.", 
  "OK: Community columns correctly coded.")
# 


# Convert community data to wide form
comm.wide <- dat$comm.long %>%
  select(-VARIABLE_UNITS) %>%
  spread(VARIABLE_NAME,  VALUE)

dat$comm.wide <- comm.wide
summary(dat)


#make subsets of data:
sites <- unique(dat$comm.long$SITE_ID)
set.seed(12)
sites <- sample(sites,length(sites), replace=F)

one.site <- subset(dat$comm.long, SITE_ID==sites[1])
five.sites <- subset(dat$comm.long, SITE_ID %in% sites[1:5])
twenty.sites <- subset(dat$comm.long, SITE_ID %in% sites[1:20])

# Calculate cumulative taxa (i.e., species accumulation) across all sites pooled together
cum.taxa.all.sites <- cuml.taxa.fun(EX = dat$comm.long)
cum.taxa.one.site <- cuml.taxa.fun(EX = one.site)
cum.taxa.five.sites <- cuml.taxa.fun(EX = five.sites)
cum.taxa.twenty.sites <- cuml.taxa.fun(EX = twenty.sites)


####
##### TEST DIFFERENT SPP ACCUM MODELS ###########
#with vegan, fit species accumulation curve:
year <- cum.taxa.all.sites$year - min(cum.taxa.all.sites$year) +1
xtmp <- seq(min(year), max(year), len=51)
## all sites:
S <- cum.taxa.all.sites$no.taxa

##The Arrhenius model:(SSarrhenius) is the expression k*area^z. This is the most classical model that can be found in any textbook of ecology (and also in Dengler 2009). Parameter z is the steepness of the species-area curve, and k is the expected number of species in a unit area.
marr <- nls( S ~SSarrhenius(year, k, z))
confint(marr) #z = steepness and k = expected number of species

#plot(S ~ year, xlab = "Time (years)", ylab = "Number of Species", ylim = c(1, max(S)))
#lines(xtmp, predict(marr, newdata=data.frame(year = xtmp)), lwd=2)

## Lomolino: using original names of the parameters (Lomolino 2000):
#The Lomolino model (SSlomolino) is Asym/(1 + slope^log(xmid/area)) (Lomolino 2000, Dengler 2009). Parameter Asym is the asymptotic maximum number of species, slope is the maximum slope of increase of richness, and xmid is the area where half of the maximum richness is achieved.
mlom <- nls(S ~ SSlomolino(year, Smax, A50, Hill))
mlom; confint(mlom)
#lines(xtmp, predict(mlom, newdata=data.frame(year=xtmp)), lwd=2, col = 4)

## Michaelis Menten:
mmic <- nls(S ~ SSmicmen(year, slope, Asym))
#lines(xtmp, predict(mmic, newdata = data.frame(year=xtmp)),lwd =2, col = 5)
mmic; confint(mmic)

## compare models (AIC)
allmods <- list(Arrhenius = marr,  Lomolino = mlom, MicMen= mmic)
sapply(allmods, AIC)
pred_all <- predict(mlom, newdata = data.frame(year=xtmp))


##### FIT THE REST  OF THE  YEARS  ###########
S_1 <- cum.taxa.one.site$no.taxa
S_5 <- cum.taxa.five.sites$no.taxa
S_20 <- cum.taxa.twenty.sites$no.taxa

#for looking at fits
#plot(year, S, pch = 19, ylim = c(0,max(cum.taxa.all.sites$no.taxa)+1), ylab = "Cumulative species richness", xlab = "Time (years)", bty="l", xaxt="n", main = "Sessile inverts at SBC LTER")
#lines(xtmp, pred_all, lwd=2)


#20 sites
#points(year, S_20, pch = 19, col=2)
mlom_20 <- nls(S_20 ~ SSlomolino(year, Smax, A50, Hill))
mmic_20 <- nls(S_20 ~ SSmicmen(year, slope, Asym))
marr_20 <- nls(S_20 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_20,  Lomolino = mlom_20, MicMen= mmic_20)
sapply(allmods, AIC)
pred_20 <- predict(mlom_20, newdata = data.frame(year=xtmp))
mlom_20; confint(mlom_20)
#lines(xtmp, predict(mlom_5, newdata=data.frame(year=xtmp)), lwd=2, col = 2)


#5 sites
#points(year, S_5, pch = 19, col=3)
cntrl <- nls.control(maxiter=300)
mlom_5 <- nls(S_5 ~ SSlomolino(year, Smax, A50, Hill), control = cntrl)
mmic_5 <- nls(S_5 ~ SSmicmen(year, slope, Asym))
marr_5 <- nls(S_5 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_5,  Lomolino = mlom_5, MicMen= mmic_5)
sapply(allmods, AIC)
pred_5 <- predict(marr_5, newdata = data.frame(year=xtmp))
marr_5; confint(marr_5)
#lines(xtmp, predict(mlom_5, newdata=data.frame(year=xtmp)), lwd=2, col = 3)


#1 site
#points(year, S_1, pch = 19, col=4)
mlom_1 <- nls(S_1 ~ SSlomolino(year, Smax, A50, Hill)) #error
mmic_1 <- nls(S_1 ~ SSmicmen(year, slope, Asym))
marr_1 <- nls(S_1 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_1, Lomolino = mlom_1, MicMen= mmic_1)
sapply(allmods, AIC)
pred_1 <- predict(mlom_1, newdata = data.frame(year=xtmp))
mlom_1; confint(mlom_1)
#lines(xtmp, predict(marr_1, newdata=data.frame(year=xtmp)), lwd=2, col = 4)


#start making figure for Box 1
pdf(file = "manuscripts/ms1/Box_1_sppAccum.pdf", height = 7, width = 7)
colours <- c("coral4", "coral3", "coral2", "coral1")
par(mfrow = c(2,2))
par(mar = c(4, 4, 0, 1) + 0.1)
plot(year, S, pch = 19, ylim = c(0,max(cum.taxa.all.sites$no.taxa)+1), ylab = "Cumulative species richness", xlab = "", xaxt = "n", bty="l",  main = "", xlim = c(0,16), col = "coral4")
lines(xtmp, pred_all, lwd=2, col = "coral4")
points(year, S_20, col="coral3", pch=19, cex=0.75)
lines(xtmp, pred_20, col="coral3")
points(year, S_5, col="coral2", pch=19, cex=0.75)
lines(xtmp, pred_5, col="coral2")
points(year, S_1, col="coral1", pch=19, cex=0.75)
lines(xtmp, pred_1, col="coral1")
axis(1, at = c (0,5,10,15))
legend("bottomright", col = colours, lty = 1, legend = c("35 sites", "20 sites","5 sites","1 site"), cex = 0.7)
text(1, 100, "A", cex=2, font = 2)
                                        #note the graphics (Quartz) window is still open

## accumulation over space
no.taxa.space <- cuml.taxa.space.fun(dat$comm.long)
num.tax <- no.taxa.space$no.taxa
site <- seq(1, length(unique(dat$comm.long$SITE_ID)),1)
mlom <- nls(num.tax ~ SSlomolino(site, Smax, A50, Hill)) 
mmic <- nls(num.tax ~ SSmicmen(site, slope, Asym))
marr <- nls(num.tax ~ SSarrhenius(site, k, z))

allmods <- list(Arrhenius = marr, Lomolino = mlom, MicMen= mmic)
sapply(allmods, AIC)

plot(rownames(no.taxa.space), no.taxa.space$no.taxa, pch = 19,  xaxt="n", bty="l", xlab = "", ylab = "", cex=0.75, ylim = c(0,max(no.taxa.space$no.taxa)+1), col = "coral4") #type = "o",
lines(site, predict(mmic, newdata=data.frame(site=site)), lwd=2, col = "coral4")
axis(side=1, at = rownames(no.taxa.space), labels = seq(1,length(no.taxa.space$site),1))

text(3, 100, "B", cex=2, font = 2)

# ---------------------------------------------------------------------------------------------------
# PANEL B: sbc-sessileInverts-castorani
L3dat <- read.csv("manuscripts/ms1/L3-sbc-sessileInverts-castorani.csv", header = T, stringsAsFactors=F)


# MAKE DATA LIST
dat <- list()

# Subset out community data and add it to dat list
dat$comm.long <- subset(L3dat, OBSERVATION_TYPE=='TAXON_COUNT')

# Ensure that community data VALUE and DATE are coded as numeric
dat$comm.long <- dat$comm.long %>%   # Recode if necessary
  mutate_at(vars(c(DATE, VALUE)), as.numeric)

  # Ensure that SITE_ID is a character: recode numeric as character 
dat$comm.long <- dat$comm.long %>%   # Recode if necessary
  mutate_at(vars(SITE_ID), as.character)

# Double-check that all columns are coded properly
ifelse(FALSE %in% 
   c(
     class(dat$comm.long$OBSERVATION_TYPE) == "character",
     class(dat$comm.long$SITE_ID) == "character",
     class(dat$comm.long$DATE) == "numeric",
     class(dat$comm.long$VARIABLE_NAME) == "character",
     class(dat$comm.long$VARIABLE_UNITS) == "character",
     class(dat$comm.long$VALUE) == "numeric"
   ),
  "ERROR: Community columns incorrectly coded.", 
  "OK: Community columns correctly coded.")

#Here, remove the first few years:
dat$comm.long <- subset(dat$comm.long, DATE > 2003)


#make subsets of data:
sites <- unique(dat$comm.long$SITE_ID)
set.seed(12)
sites <- sample(sites,length(sites), replace=F)

one.site <- subset(dat$comm.long, SITE_ID==sites[1])
two.sites <- subset(dat$comm.long, SITE_ID %in% sites[1:2])
five.sites <- subset(dat$comm.long, SITE_ID %in% sites[1:5])
eleven.sites <- subset(dat$comm.long, SITE_ID %in% sites[1:11])

# Calculate cumulative taxa (i.e., species accumulation) across all sites pooled together
cum.taxa.all.sites <- cuml.taxa.fun(EX = dat$comm.long)
cum.taxa.one.site <- cuml.taxa.fun(EX = one.site)
cum.taxa.two.sites <- cuml.taxa.fun(EX = two.sites)
cum.taxa.five.sites <- cuml.taxa.fun(EX = five.sites)

#plot(cum.taxa.all.sites$year, cum.taxa.all.sites$no.taxa, type = "o", pch = 19, ylim = c(0,max(cum.taxa.all.sites$no.taxa)+1), ylab = "Cumulative species richness", xlab = "Time (years)", bty="l", xaxt="n", main = "Sessile inverts at SBC LTER")
#points(cum.taxa.five.sites$year, cum.taxa.five.sites$no.taxa, type = "o", pch = 19, lty=2)
#points(cum.taxa.two.sites$year, cum.taxa.two.sites$no.taxa, type = "o", pch = 19, lty=3)
#points(cum.taxa.one.site$year, cum.taxa.one.site$no.taxa, type = "o", pch = 19, lty=4)
#legend("bottomright", lty = c(1,2,3,4), legend = c("11 sites", "five sites","two sites","one site"))
#axis(side = 1, at = c( 2004, 2009, 2014), labels = c(1,5,10))

####
##### TEST DIFFERENT SPP ACCUM MODELS ###########
#with vegan, fit species accumulation curve:
year <- cum.taxa.all.sites$year - min(cum.taxa.all.sites$year) +1
xtmp <- seq(min(year), max(year), len=51)
## all sites:
S <- cum.taxa.all.sites$no.taxa

##The Arrhenius model:(SSarrhenius) is the expression k*area^z. This is the most classical model that can be found in any textbook of ecology (and also in Dengler 2009). Parameter z is the steepness of the species-area curve, and k is the expected number of species in a unit area.
marr <- nls( S ~SSarrhenius(year, k, z))
confint(marr) #z = steepness and k = expected number of species

#plot(S ~ year, xlab = "Time (years)", ylab = "Number of Species", ylim = c(1, max(S)))
#lines(xtmp, predict(marr, newdata=data.frame(year = xtmp)), lwd=2)

## Lomolino: using original names of the parameters (Lomolino 2000):
#The Lomolino model (SSlomolino) is Asym/(1 + slope^log(xmid/area)) (Lomolino 2000, Dengler 2009). Parameter Asym is the asymptotic maximum number of species, slope is the maximum slope of increase of richness, and xmid is the area where half of the maximum richness is achieved.
mlom <- nls(S ~ SSlomolino(year, Smax, A50, Hill))
mlom; confint(mlom)
#lines(xtmp, predict(mlom, newdata=data.frame(year=xtmp)), lwd=2, col = 4)

## Michaelis Menten:
mmic <- nls(S ~ SSmicmen(year, slope, Asym))
#lines(xtmp, predict(mmic, newdata = data.frame(year=xtmp)),lwd =2, col = 5)
mmic; confint(mmic)

## compare models (AIC)
allmods <- list(Arrhenius = marr,  Lomolino = mlom, MicMen= mmic)
sapply(allmods, AIC)
pred_all <- predict(mlom, newdata = data.frame(year=xtmp))


##### FIT THE REST  OF THE  YEARS  ###########
S_1 <- cum.taxa.one.site$no.taxa
S_2 <- cum.taxa.two.sites$no.taxa
S_5 <- cum.taxa.five.sites$no.taxa

#for looking at fits
#plot(year, S, pch = 19, ylim = c(0,max(cum.taxa.all.sites$no.taxa)+1), ylab = "Cumulative species richness", xlab = "Time (years)", bty="l", xaxt="n", main = "Sessile inverts at SBC LTER")
#lines(xtmp, pred_all, lwd=2)


#5 sites
#points(year, S_5, pch = 19, col=2)
mlom_5 <- nls(S_5 ~ SSlomolino(year, Smax, A50, Hill))
mmic_5 <- nls(S_5 ~ SSmicmen(year, slope, Asym))
marr_5 <- nls(S_5 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_5,  Lomolino = mlom_5, MicMen= mmic_5)
sapply(allmods, AIC)
pred_5 <- predict(mlom_5, newdata = data.frame(year=xtmp))
mlom_5; confint(mlom_5)
#lines(xtmp, predict(mlom_5, newdata=data.frame(year=xtmp)), lwd=2, col = 2)


#2 sites
#points(year, S_2, pch = 19, col=3)
mlom_2 <- nls(S_2 ~ SSlomolino(year, Smax, A50, Hill))
mmic_2 <- nls(S_2 ~ SSmicmen(year, slope, Asym))
marr_2 <- nls(S_2 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_2,  Lomolino = mlom_2, MicMen= mmic_2)
sapply(allmods, AIC)
pred_2 <- predict(mlom_2, newdata = data.frame(year=xtmp))
mlom_2; confint(mlom_2)
#lines(xtmp, predict(mlom_2, newdata=data.frame(year=xtmp)), lwd=2, col = 3)


#1 site
#points(year, S_1, pch = 19, col=4)
mlom_1 <- nls(S_1 ~ SSlomolino(year, Smax, A50, Hill)) #error
mmic_1 <- nls(S_1 ~ SSmicmen(year, slope, Asym))
marr_1 <- nls(S_1 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_1,  MicMen= mmic_1) #Lomolino = mlom_1, 
sapply(allmods, AIC)
pred_1 <- predict(marr_1, newdata = data.frame(year=xtmp))
marr_1; confint(marr_1)
#lines(xtmp, predict(marr_1, newdata=data.frame(year=xtmp)), lwd=2, col = 4)


#plot
par(mar = c(4, 4, 0, 1) + 0.1)
colours <- c("cadetblue4", "cadetblue3", "cadetblue2", "cadetblue1")
plot(year, S, pch = 19, ylim = c(0,max(cum.taxa.all.sites$no.taxa)+1), ylab = "Cumulative species richness", xlab = "Time (years)", bty="l",  main = "", xlim = c(0,16), col = "cadetblue4")
lines(xtmp, pred_all, lwd=2, col = "cadetblue4")
points(year, S_5, col="cadetblue3", pch=19, cex=0.75)
lines(xtmp, pred_5, col="cadetblue3")
points(year, S_2, col="cadetblue2", pch=19, cex=0.75)
lines(xtmp, pred_2, col="cadetblue2")
points(year, S_1, col="cadetblue1", pch=19, cex=0.75)
lines(xtmp, pred_1, col="cadetblue1")
legend("bottomright", col = colours, lty = 1, legend = c("11 sites", "5 sites","2 sites","1 site"), cex=0.7)
text(1, 65, "C", cex=2, font = 2)



## accumulation over space
no.taxa.space <- cuml.taxa.space.fun(dat$comm.long)
num.tax <- no.taxa.space$no.taxa
site <- seq(1, length(unique(dat$comm.long$SITE_ID)),1)
mlom <- nls(num.tax ~ SSlomolino(site, Smax, A50, Hill)) 
mmic <- nls(num.tax ~ SSmicmen(site, slope, Asym))
marr <- nls(num.tax ~ SSarrhenius(site, k, z))

allmods <- list(Arrhenius = marr, Lomolino = mlom, MicMen= mmic)
sapply(allmods, AIC)

plot(rownames(no.taxa.space), no.taxa.space$no.taxa, pch = 19,  xaxt="n", bty="l", xlab = "", ylab = "", cex=1, ylim = c(0,max(no.taxa.space$no.taxa)+1), col = "cadetblue4") #type = "o",
lines(site, predict(mmic, newdata=data.frame(site=site)), lwd=2, col = "cadetblue4")
axis(side=1, at = rownames(no.taxa.space), labels = seq(1,length(no.taxa.space$site),1))

text(1.5, 65, "D", cex=2, font = 2)

dev.off()




# FIGURE 2: Succession and species invasions ---------------------------------------------------------------------------------------------------
# PANEL A: and-plants-mtStHelens
L3dat <- read.csv("manuscripts/ms1/L3-and-plants-mtStHelens.csv", header = T, stringsAsFactors=F)


# MAKE DATA LIST
dat <- list()

# Subset out community data and add it to dat list
dat$comm.long <- subset(L3dat, OBSERVATION_TYPE=='TAXON_COUNT')

# Ensure that community data VALUE and DATE are coded as numeric
dat$comm.long <- dat$comm.long %>%   # Recode if necessary
  mutate_at(vars(c(DATE, VALUE)), as.numeric)

  # Ensure that SITE_ID is a character: recode numeric as character 
dat$comm.long <- dat$comm.long %>%   # Recode if necessary
  mutate_at(vars(SITE_ID), as.character)

# Double-check that all columns are coded properly
ifelse(FALSE %in% 
   c(
     class(dat$comm.long$OBSERVATION_TYPE) == "character",
     class(dat$comm.long$SITE_ID) == "character",
     class(dat$comm.long$DATE) == "numeric",
     class(dat$comm.long$VARIABLE_NAME) == "character",
     class(dat$comm.long$VARIABLE_UNITS) == "character",
     class(dat$comm.long$VALUE) == "numeric"
   ),
  "ERROR: Community columns incorrectly coded.", 
  "OK: Community columns correctly coded.")


#make subsets of data:
sites <- unique(dat$comm.long$SITE_ID)
set.seed(12)
sites <- sample(sites,length(sites), replace=F)

one.site <- subset(dat$comm.long, SITE_ID==sites[1])
two.sites <- subset(dat$comm.long, SITE_ID %in% sites[1:2])
six.sites <- subset(dat$comm.long, SITE_ID %in% sites[1:6])

# Calculate cumulative taxa (i.e., species accumulation) across all sites pooled together
cum.taxa.all.sites <- cuml.taxa.fun(EX = dat$comm.long)
cum.taxa.one.site <- cuml.taxa.fun(EX = one.site)
cum.taxa.two.sites <- cuml.taxa.fun(EX = two.sites)
cum.taxa.six.sites <- cuml.taxa.fun(EX = six.sites)


####
##### TEST DIFFERENT SPP ACCUM MODELS ###########
#with vegan, fit species accumulation curve:
year <- cum.taxa.all.sites$year - min(cum.taxa.all.sites$year) +1
xtmp <- seq(min(year), max(year), len=51)
## all sites:
S <- cum.taxa.all.sites$no.taxa

##The Arrhenius model:(SSarrhenius) is the expression k*area^z. This is the most classical model that can be found in any textbook of ecology (and also in Dengler 2009). Parameter z is the steepness of the species-area curve, and k is the expected number of species in a unit area.
marr <- nls( S ~SSarrhenius(year, k, z))
confint(marr) #z = steepness and k = expected number of species

#plot(S ~ year, xlab = "Time (years)", ylab = "Number of Species", ylim = c(1, max(S)))
#lines(xtmp, predict(marr, newdata=data.frame(year = xtmp)), lwd=2)

## Lomolino: using original names of the parameters (Lomolino 2000):
#The Lomolino model (SSlomolino) is Asym/(1 + slope^log(xmid/area)) (Lomolino 2000, Dengler 2009). Parameter Asym is the asymptotic maximum number of species, slope is the maximum slope of increase of richness, and xmid is the area where half of the maximum richness is achieved.
mlom <- nls(S ~ SSlomolino(year, Smax, A50, Hill))
mlom; confint(mlom)
#lines(xtmp, predict(mlom, newdata=data.frame(year=xtmp)), lwd=2, col = 4)

## Michaelis Menten:
mmic <- nls(S ~ SSmicmen(year, slope, Asym))
#lines(xtmp, predict(mmic, newdata = data.frame(year=xtmp)),lwd =2, col = 5)
mmic; confint(mmic)

## compare models (AIC)
allmods <- list(Arrhenius = marr,  Lomolino = mlom, MicMen= mmic)
sapply(allmods, AIC)
pred_all <- predict(marr, newdata = data.frame(year=xtmp))


##### FIT THE REST  OF THE  YEARS  ###########
S_1 <- cum.taxa.one.site$no.taxa
S_2 <- cum.taxa.two.sites$no.taxa
S_6 <- cum.taxa.six.sites$no.taxa

#for looking at fits
#plot(year, S, pch = 19, ylim = c(0,max(cum.taxa.all.sites$no.taxa)+1), ylab = "Cumulative species richness", xlab = "Time (years)", bty="l", xaxt="n", main = "Mt St Helens")
#lines(xtmp, pred_all, lwd=2)


#6 sites
#points(year, S_6, pch = 19, col=2)
cntrl <- nls.control(maxiter=300)
mlom_6 <- nls(S_6 ~ SSlomolino(year, Smax, A50, Hill), control = cntrl)
mmic_6 <- nls(S_6 ~ SSmicmen(year, slope, Asym))
marr_6 <- nls(S_6 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_6,  MicMen= mmic_6)
sapply(allmods, AIC)
pred_6 <- predict(marr_6, newdata = data.frame(year=xtmp))
marr_6; confint(marr_6)
#lines(xtmp, predict(marr_6, newdata=data.frame(year=xtmp)), lwd=2, col = 2)


#2 sites
#points(year, S_2, pch = 19, col=3)
mlom_2 <- nls(S_2 ~ SSlomolino(year, Smax, A50, Hill), control = cntrl)
mmic_2 <- nls(S_2 ~ SSmicmen(year, slope, Asym))
marr_2 <- nls(S_2 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_2,  Lomolino = mlom_2, MicMen= mmic_2)
sapply(allmods, AIC)
pred_2 <- predict(marr_2, newdata = data.frame(year=xtmp))
marr_2; confint(marr_2)
#lines(xtmp, predict(marr_2, newdata=data.frame(year=xtmp)), lwd=2, col = 3)


#1 site
#points(year, S_1, pch = 19, col=4)
mlom_1 <- nls(S_1 ~ SSlomolino(year, Smax, A50, Hill)) #error
mmic_1 <- nls(S_1 ~ SSmicmen(year, slope, Asym))
marr_1 <- nls(S_1 ~ SSarrhenius(year, k, z))

allmods <- list(Arrhenius = marr_1,  MicMen= mmic_1, Lomolino = mlom_1) 
sapply(allmods, AIC)
pred_1 <- predict(mmic_1, newdata = data.frame(year=xtmp))
mmic_1; confint(mmic_1)
#lines(xtmp, predict(mmic_1, newdata=data.frame(year=xtmp)), lwd=2, col = 4)


#start making Figure 2 (don't plot fits, it's succession.)
pdf(file = "manuscripts/ms1/Box_1_Fig2.pdf", height = 3.5, width = 7)
colours = c("coral4", "coral3", "coral2", "coral1")
par(mfrow = c(1,2))
par(mar = c(4, 4, 0, 1) + 0.1)
plot(year, S, pch = 19, ylim = c(0,max(cum.taxa.all.sites$no.taxa)+1), ylab = "Cumulative species richness", xlab = "Time (years)", bty="l",  main = "", type = "o", col = "coral4")
#lines(xtmp, pred_all, lwd=2)
points(year, S_6, pch=19, cex=0.75, type = "o", col = "coral3")
#lines(xtmp, pred_6, col="coral3")
points(year, S_2, col="coral2", pch=19, cex=0.75, type = "o")
#lines(xtmp, pred_2, col="coral2)
points(year, S_1, col="coral1", pch=19, cex=0.75, type = "o")
#lines(xtmp, pred_1, col="coral1)
legend("bottomright", col = colours, lty = 1, legend = c("12 sites", "6 sites","2 sites","1 site"), cex=0.75, lwd = c(2,1,1,1))
text(2, 39, "A", cex=2, font = 2)


## accumulation over space
no.taxa.space <- cuml.taxa.space.fun(dat$comm.long)

plot(rownames(no.taxa.space), no.taxa.space$no.taxa, pch = 19, type = "o", xaxt="n", bty="l", xlab = "Cumulative number of sites", ylab = "", cex=0.75, ylim = c(0,max(no.taxa.space$no.taxa)+1))

axis(side=1, at = rownames(no.taxa.space), labels = seq(1,length(no.taxa.space$site),1))

text(2, 39, "B", cex=2, font = 2)

dev.off()

