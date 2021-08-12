# Lamy et al. (2021) The dual nature of metacommunity stability, OIKOS, doi: 10.1111/oik.08517

space_stab <- function(Y, s, t, plot=FALSE)
#
# Description --
# R function to partition aggregate and compositional variability across spatial scales in a multiplicative framework
# The decomposition of aggregate variability across spatial scales is based on Wang and Loreau (2014)
# The decomposition of compositional variability across spatial scales is based on the Hellinger distance
#
# Arguments --
#
# Y: community table: an observation by species matrix with row blocks corresponding to local communites
# s: number of sites
# t: number of years
#
# Outputs --
# AlhaCV  : aggregate variability at local scale (alpha aggregate variability)
# GammaCV : aggregate variability at regional scale (gamma aggregate variability)
# phiCV   : spatial aggregate synchrony
#
# AlhaHBD  : compositional variability at local scale (alpha compositional variability)
# GammaHBD : compositional variability at regional scale (gamma compositional variability)
# phiHBD   : spatial compositional synchrony defined as the ratio between GammaHBD and AlhaHBD
#
# Dependence / required package --
#
# {reshape2} and {ggplot2} 
#
# Reference --
#
# Wang and Loreau (2014) Ecosystem stability in space: α, β and γ variability.
# Ecology Letters. 17: 891-901.
# 
# Author: Thomas LAMY
# edited May 10th 2018
{
# check that the data is balanced
if(dim(Y)[1] != s * t) stop("Sites are not surveyed every years")
# number of species
nbsp <- dim(Y)[2]
# Total aggregate metric
tot.bio <- apply(Y, 1, sum)
# List of local communities
SiteL <- list(); for(i in 1:s) SiteL[[i]] <- Y[c(((i-1)*t+1):(i*t)),] 
# Metacommunity, sum across all local communities
MetacomTime <- Reduce("+", SiteL) 

#### Partitioning aggregate variability across scales 
# Matrix of total aggregate community metric with local communities as row and time as colums
Nit <- matrix(tot.bio, nrow=s, ncol=t, byrow=TRUE)
# Variance-covariance matrix of the total aggregate community metric across local communities
W <- cov(t(Nit))
# Temporal mean aggregate metric of each local community
mui <- apply(Nit, 1, mean)
# Temporal standard deviation of the aggregate metric of each local community 
sigi <- sqrt(diag(W))
# Temporal aggregate variability of each local community 
CVi <- sigi/mui
# Temporal mean of the aggregate metric of the whole metacommunity
muM <- sum(mui)
# Temporal standard deviation of the aggregate metric of the whole metacommunity
# (sqrt of the sum of the temporal covariances between local communities)
sigM <- sqrt(sum(W))
# Temporal variability at the local scale is defined as the weighted average of CVi across local communities
CVL <- sum(sigi)/muM  # = sum(mui/muM*CVi)
# Temporal variability at the metacommunity scale is the coefficient of temporal variation of metacommunity aggregate metric
CVM <- sigM/muM
# GammaCV = AlphaCV/BetaCV; with BetaCV = 1/PhiCV with BetaCV the spatial asynchrony-related aggregate variability
# We define the squared coefficients of variation at the metacommunity scale as gamma aggregate variability
GammaCV <-  CVM^2
# We define the squared coefficients of variation at local scale as alpha aggregate variability
AlphaCV <- CVL^2
PhiCV <- (sigM^2)/((sum(sigi))^2) # = CVM^2/CVL^2 

#### Partitioning compositional variability across scales
# Function to compute compositional variability based on the Hellinger distance
HBDfun <- function(Y){
  # Hellinger transformation
  YY <- sqrt(Y/apply(Y,1,sum))
  YY[is.na(YY)] <- 0    # if one site has no species a given year this makes sure to return 0
  # square of the mean differences
  s <- scale(YY, center=TRUE, scale=FALSE)^2 
  # total sum of squares in the species composition
  SStotal <- sum(s)
  # compute beta total based on the Hellinger distance
  n <- dim(Y)[1]
  HBD <- SStotal/(n-1)
  return(HBD)
}
# Compositional variability of each local communities
HBDi <- lapply(SiteL, function(x) {HBDfun(x)})
HBDi <- do.call(c, HBDi)
# Local community weights
w_i <- mui/muM
# Alpha compositional variability
AlphaHBD <- sum(w_i * HBDi)
# Gamma compositional variability
GammaHBD <- HBDfun(MetacomTime)
PhiHBD <- GammaHBD/AlphaHBD

#### plot species and total community biomass across sites and over time
if(plot==TRUE){ 
  Yplot <- melt(rbind(
      cbind(as.data.frame(do.call(rbind, SiteL)), time=rep(seq(1,t,1),s), site=as.factor(rep(1:s, each=t))), 
      cbind(as.data.frame(MetacomTime), time=seq(1,t,1), site=rep("Region", t))), id.vars = c("time", "site"))
  pp <- ggplot(Yplot, aes(x=time, y=value, fill=variable)) + geom_area(stat="identity") +
    facet_wrap(~site) + xlab("Time") + ylab("Biomass") + ggtitle("") + theme_bw()
  print(pp)
}

#### summary table
res <- data.frame(GammaCV=GammaCV, AlphaCV=AlphaCV, PhiCV=PhiCV,
                  GammaHBD=GammaHBD, AlphaHBD=AlphaHBD, PhiHBD=PhiHBD)
res
}



