aggregate_stab <- function(Y, s, t)
#
# function to compute alpha, beta and gamma aggregate stability
#
# Arguments --
#
# Y: community table: assumes row blocks corresponding to times
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

# compute total community biomass
tot.bio <- apply(Y, 1, sum)

# check that data is balanced
if(length(tot.bio) != s * t) cat("STOP: sites are not surveyed every years")

# matrix of Ni(t) with sites as row and time as colums
Nit <- matrix(tot.bio, nrow=s, ncol=t, byrow=TRUE)

# compute the average temporal biomass at each site
mui <- apply(Nit, 1, mean)

# compute the variance-covariance matrix of temporal biomass across sites [W = wij of N(t)]
W <- cov(t(Nit))

# get the temporal variance (sd) of the communities in each of the m sites
Vari <- diag(W)
sigi <- sqrt(diag(W))
# get the sum of the temporal covariances of the communities in each of the m sites
Covsum <- sum(W[lower.tri(W, diag=FALSE)])

#### local scale temporal variability ####
# temporal variability of the ith community is calculated as the coefficient of temporal variation of its biomass
CVi <- sigi/mui
# compute the temporal mean of the total metacommunity biomass
muM <- sum(mui)
# temporal variability at the local scale is defined as the weighted average of CVi across local sites
CVL <- sum(sigi)/muM  # = sum(mui/muM*CVi)
# local variability is the squared coefficient of temporal variations at local scale
CVL2 <- CVL^2

#### Metacommunity/regional scale temporal variability ####
# temporal variability at the metacommunity scale is the coefficient of temporal variation of metacommunity biomass
CVM <- sqrt(Covsum)/muM
# Metacommunity variability is the squared coefficient of temporal variations at metacommunity scale
CVM2 <- CVM^2

#### Spatial variability at the metacommunity scale ####
# compute the average biomass of local communities at each time t
Nbart <- apply(Nit, 2, mean)
# At each time t we can compute the spatial variance among local sites
VSt <- c(); for (i in 1:t) VSt[i] <- sum((Nit[,i] - Nbart[i])^2)/s

# mubar is the spatial average of the temporal mean of local community biomass
mubar <- mean(mui)
## spatial average of the standard deviation of local community biomass
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
res <- data.frame(variable = c("Gamma", "Alpha", "Beta", "Phi"), val=c(GammaCV, AlphaCV, Beta1, phi))

res
}


