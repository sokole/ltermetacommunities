# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

aggregate_stab <- function(Y, s, t, plot = FALSE)
#
# function to compute alpha, beta and gamma aggregate stability
#
# Arguments --
#
# Y: community table: assumes row blocks corresponding to local communities
# s: number of site
# t: number of years
#
# Outputs --
# alpha variability = GammaCV 
# gamma variability = GammaCV
# beta variability = beta1 (in multiplicative framework) beta2 (in additive framework)
# phi.agg = spatial asynchrony in total aggregate community property
#
#
# Reference --
#
# Wang and Loreau (2014) Ecosystem stability in space: α, β and γ variability.
# Ecology Letters. 17: 891-901.
# 
# Author: Thomas LAMY
# edited September 28th 2016
{

# compute total community biomass (i.e. the aggregate community metric)
tot.bio <- apply(Y, 1, sum)

# check that data is balanced
if(length(tot.bio) != s * t) cat("STOP: sites are not surveyed every years")

# matrix of Ni(t) with sites as row and time as colums
Nit <- matrix(tot.bio, nrow=s, ncol=t, byrow=TRUE)

# variance-covariance matrix of the temporal biomass across sites [W = wij of N(t)]
W <- cov(t(Nit))

#### local scale aggregate variability ####
# compute the average total biomass at each site
mui <- apply(Nit, 1, mean)

# get the variance, and sd, of the total biomass in each of the s sites
Vari <- diag(W)
sigi <- sqrt(diag(W))

# temporal variability of the ith community is calculated as the coefficient of temporal variation of its biomass
CVi <- sigi/mui

# compute the temporal mean of the total metacommunity biomass
muM <- sum(mui)

# temporal variability at the local scale is defined as the weighted average of CVi across local sites
CVL <- sum(sigi)/muM  # = sum(mui/muM*CVi)
# local aggregate variability is the squared coefficient of temporal variations at local scale
CVL2 <- CVL^2

#### Metacommunity/regional scale temporal variability ####
# get the sum of the temporal covariances of the communities in each of the m sites
Covsum <- sum(W)
# temporal variability at the metacommunity scale is the coefficient of temporal variation of metacommunity biomass
CVM <- sqrt(Covsum)/muM
# Metacommunity aggregate variability is the squared coefficient of temporal variations at metacommunity scale
CVM2 <- CVM^2

## Total biomass at the metacommunity level
NiM <- apply(Nit, 2, sum)

# Metacommunity aggregate variability can also be compute based on:
# metacommunity temporal variance
# var(NiM) # = Covsum <- sum(W)
# temporal average of the total metacommunity biomass
# mean(NiM) # = muM
## whereby regional CV
# sqrt(var(NiM))/mean(NiM)
# CVM2 = var(NiM)/mean(NiM)^2

# plot total community biomass across sites and over time
if(plot){ 
  plot(1:t, NiM, type="l", ylab="Total community biomass", xlab="Time", main="", lwd = 4, ylim = c(0, max(NiM)))
  coli <- rainbow(s)
  for (i in 1:s) lines(Nit[i,], lwd = 2, lty = 2, col = coli[i])	
}

#### Spatial variability at the metacommunity scale ####
# compute the average biomass of local communities at each time t
Nbart <- apply(Nit, 2, mean)
# At each time t we can compute the spatial variance among local sites
VSt <- c(); for (i in 1:t) VSt[i] <- sum((Nit[,i] - Nbart[i])^2)/s

# mubar is the spatial average of the temporal mean of local community biomass
mubar <- mean(mui)
# spatial average of the standard deviation of local community biomass
sqrtwbar <- mean(sigi) 
# We now compute spatial variability as
CVS2 <- 1/(mubar^2)*(sqrtwbar^2 - ((1/s^2)*Covsum) + (1/s)*sum((mui - mubar)^2 + (sigi - sqrtwbar)^2))

#### Linking variability across multiple scales: multiplicative partitionning ####
# We define the squared coefficients of variation at regional scale as gamma variability
GammaCV <-  CVM2
# We define the squared coefficients of variation at local scale as alpha variability
AlphaCV <- CVL2
# Multiplicative partitionning of metacommunity stability
# GammaCV = AlphaCV / Beta1
# with Beta1 = 1/phi the spatial asynchrony-related variability
# phi is the index of spatial synchrony which serves as a scaling factor that scales temporal variability from local 
# to regional scales (phi = CVM2/CVL2)
# The more local communities fluctuate synchronously, the higher the variability of the metacommunity.
# phi takes values between 0 and 1
phi <- Covsum/((sum(sigi))^2)
# should verify phi = CVM2/CVL2 
# round(phi, 4) == round(CVM2/CVL2, 4)
Beta1 <- 1/phi ##  spatial asynchrony-related variability

# summary result
res <- data.frame(GammaCV = GammaCV, AlphaCV = AlphaCV, PhiCV = phi)

res
}


