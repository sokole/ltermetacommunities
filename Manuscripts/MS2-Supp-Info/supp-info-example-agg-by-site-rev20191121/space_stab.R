# Lamy et al. (in prep) The dual nature of metacommunity stability
# Appendix SX

space_stab <- function(Y, s, t, plot=FALSE)
#
# Description --
# R function to partition aggregate and compositional variability across spatial scales in a multiplicative framework
# The decomposition of aggregate variability across spatial scales is based on Wang and Loreau (2014)
# The decomposition of compositional variability across spatial scales is based on three approaches
# depending on how temporal compositional variability (TCV) is defined:
  #   TCV is based on a time-independent Hellinger transformation,
  #     which is based on the temporal variance of the square root transformed data       
  #   TCV is based on the Hellinger distance
  #   TCV is based on the species temporal variances
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
# betaCV  : aggregate variability among sites corresponding to beta1 in Wang and Loreau (2014)
# phiCV   : spatial aggregate synchrony, or spatial synchrony in the aggregate community property
#
# AlhaTBD  : TCV at local scale (alpha compositional variability) based on a the time-independent Hellinger transformation
# GammaTBD : TCV at regional scale (gamma compositional variability) based on a the time-independent Hellinger transformation
# betaTBD  : TCV among sites based on a the time-independent Hellinger transformation
# phiTBD   : spatial compositional synchrony based on a the time-independent Hellinger transformation
#
# AlhaHBD  : TCV at local scale (alpha compositional variability) based on a the Hellinger distance
# GammaHBD : TCV at regional scale (gamma compositional variability) based on a the Hellinger distance
# betaHBD  : TCV among sites based on a the Hellinger distance
# phiHBD   : spatial compositional synchrony based on a the Hellinger distance
#
# AlphaVAR  : TCV at local scale (alpha compositional variability) based on species variance
# GammaVAR  : TCV at regional scale (gamma compositional variability) based on species variance
# betaVAR   : TCV among sites based on species variance
# phiVAR    : spatial compositional synchrony based on species variance
#
# Dependence / required package --
#
# Reference --
#
# Wang and Loreau (2014) Ecosystem stability in space: α, β and γ variability.
# Ecology Letters. 17: 891-901.
# 
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
SiteL <- list(); for(i in 1:s) SiteL [[i]] <- Y[c(((i-1)*t+1):(i*t)),] 
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
# Spatial asynchrony-related variability
BetaCV <- 1/PhiCV 


#### Partitioning compositional variability across scales
    # TCV is based on a time-independent Hellinger transformation,
    # which is based on the temporal variance of the square root transformed data       

## Way 1: through species temporal variance of species sqrt biomass
# temporal variance of each sqrt species in each local community
var_ij_sqrt <- lapply(SiteL, function(x){apply(sqrt(x), 2, var)})
# Temporal variance of each sqrt species at the metacommunity scale 
var_Tj_sqrt <- apply(sqrt(MetacomTime), 2, var)
# TCV of each local communities
TBDi <- unlist(lapply(var_ij_sqrt, sum))/unlist(lapply(SiteL, sum))
# Alpha compositional variability
AlphaTBD <- sum(unlist(lapply(var_ij_sqrt, sum)))/sum(MetacomTime)
# Gamma compositional variability
GammaTBD <- sum(var_Tj_sqrt)/sum(MetacomTime)

# Species-specific spatial synchrony 
phiTBD_j <- c()
for (j in 1:nbsp) phiTBD_j[j] <- var_Tj_sqrt[[j]] / sum(do.call(c, lapply(var_ij_sqrt, function(x){ x[j] })))
# range(phiTBD_j)
# species weights
w_j_sqrt <- c()
for (j in 1:nbsp) w_j_sqrt[j] <- sum(do.call(c, lapply(var_ij_sqrt, function(x){ x[j] })))
w_j_sqrt <- w_j_sqrt/sum(w_j_sqrt)
# PhiTBD <- sum(w_j_sqrt * phiTBD_j)

## Way 2: through internal function
# Function to compute TCV based on the time-independent Hellinger transformation
TBDfun <- function(Y){
  # time-independent Hellinger transformation
  YY <- sqrt(Y/sum(Y))
  # square of the mean differences
  s <- scale(YY, center=TRUE, scale=FALSE)^2 
  # total sum of squares in the species composition
  SStotal <- sum(s)
  # compute beta total based on the time-independent Hellinger transformation
  n <- dim(Y)[1]
  TBD <- SStotal/(n-1)
  return(TBD)
}
# TCV of each local communities
TBDi <- lapply(SiteL, function(x) {TBDfun(x)})
TBDi <- do.call(c, TBDi)
# Local community weights
w_i <- mui/muM
# Alpha compositional variability
AlphaTBD <- sum(w_i * TBDi)
# Gamma compositional variability
GammaTBD <- TBDfun(MetacomTime)
# GammaTBD = AlphaTBD/BetaTBD; with BetaTBD = 1/phiTBD the spatial asynchrony-related compositional variability
PhiTBD <- GammaTBD/AlphaTBD
BetaTBD <- 1/PhiTBD

#### Partitioning compositional variability across scales
    # TCV is based on the Hellinger distance
# Function to compute TCV based on the Hellinger distance
HBDfun <- function(Y){
  # Hellinger transformation
  YY <- sqrt(Y/apply(Y,1,sum))
  # square of the mean differences
  s <- scale(YY, center=TRUE, scale=FALSE)^2 
  # total sum of squares in the species composition
  SStotal <- sum(s)
  # compute beta total based on the Hellinger distance
  n <- dim(Y)[1]
  HBD <- SStotal/(n-1)
  return(HBD)
}
# TCV of each local communities
HBDi <- lapply(SiteL, function(x) {HBDfun(x)})
HBDi <- do.call(c, HBDi)
# Alpha compositional variability
AlphaHBD <- sum(w_i * HBDi)
# Gamma compositional variability
GammaHBD <- HBDfun(MetacomTime)
# GammaHBD = AlphaHBD/BetaHBD; with BetaHBD = 1/phiHBD the spatial asynchrony-related compositional variability
PhiHBD <- GammaHBD/AlphaHBD
BetaHBD <- 1/PhiHBD

#### Partitioning compositional variability across scales
    # TCV is based on the species temporal variances
# temporal mean biomass of each species in each local community
mu_ij <- lapply(SiteL, function(x){apply(x, 2, mean)})
# Temporal variance of each species in each local community
var_ij <- lapply(SiteL, function(x){apply(x, 2, var)})
# Temporal mean biomass of the whole community with each local community
mu_Ti <- lapply(mu_ij, function(x){sum(x)}) # = lapply((lapply(SiteL, function(x){apply(x, 1, sum)})), mean)
# Temporal mean biomass of the whole metacommunity
mu_TT <- sum(do.call(c, mu_Ti)) # mean(apply(MetacomTime, 1, sum)) = muM from above
# Temporal mean biomass of each species at the metacommunity scale
mu_Tj <- apply(MetacomTime, 2, mean)
# Temporal variance of each species at the metacommunity scale 
var_Tj <- apply(MetacomTime, 2, var)
# Alpha compositional variability
AlphaVAR <- sum(do.call(c, lapply(var_ij, function(x){sum(sqrt(x))})))^2 / mu_TT^2
# Gamma compositional variability
GammaVAR <- sum(sqrt(var_Tj))^2/mu_TT^2
# GammaVAR = AlphaVAR/BetaVAR; with BetaVAR = 1/phiVAR the spatial asynchrony-related compositional variability
PhiVAR <- GammaVAR/AlphaVAR # = sum(sqrt(var_Tj))^2/sum(do.call(c, lapply(var_ij, function(x){sum(sqrt(x))})))^2
BetaVAR <- 1/PhiVAR
# Specie-specific spatial synchrony
PhiVAR_j <- c()
for (j in 1:nbsp) PhiVAR_j[j] <- var_Tj[[j]]/sum(do.call(c, lapply(var_ij, function(x){ sqrt(x[j]) })))^2
# range(PhiVAR_j)
# Species weights
w_j <- c()
for (j in 1:nbsp) w_j[j] <- sum(do.call(c, lapply(var_ij, function(x){ sqrt(x[j]) })))
w_j <- w_j/sum(w_j)
# sum(w_j * sqrt(PhiVAR_j))^2


#### plot species and total community biomass across sites and over time
if(plot){ 
  Yplot <- melt(rbind(
      cbind(as.data.frame(do.call(rbind, SiteL)), time=rep(seq(1,t,1),s), site=as.factor(rep(1:s, each=t))), 
      cbind(as.data.frame(MetacomTime), time=seq(1,t,1), site=rep("Region", t))), id.vars = c("time", "site"))
  ggplot(Yplot, aes(x = time, y = value, fill = variable)) + geom_area(stat = "identity") +
    facet_wrap( ~ site) + xlab("Time") + ylab("Biomass") + ggtitle("") + theme_bw()
}

#### summary table
res <- data.frame(GammaCV = GammaCV, AlphaCV = AlphaCV, PhiCV = PhiCV,
                  GammaTBD = GammaTBD, AlphaTBD = AlphaTBD, PhiTBD = PhiTBD,
                  GammaHBD = GammaHBD, AlphaHBD = AlphaHBD, PhiHBD = PhiHBD,
                  GammaVAR = GammaVAR, AlphaVAR = AlphaVAR, PhiVAR = PhiVAR)
res
}



