# Script to draw different scenarios of aggregagte and compositional community variation across scales
# Thomas Lamy
# March 25th 2017

# load packages
library(tidyverse)
library(reshape2)
library(ggplot2)
library(ade4)
library(vegan)

# load function Aggregate_stability.R and Compositional_stability.R
source("Group3-diversity-metrics/Aggregate_stability.R")
source("Group3-diversity-metrics/Compositional_stability.R")

# load function beta.div.comp
source("utilities/beta_diversity_function/beta.div.comp.R")

# function correlated.matrix from the synchrony package (Tarik C. Gouhier)
# Create an n times x nspecies matrix with correlation ρ, standard deviation σ and mean μ
#
correlated.matrix <- function (rho = 0, sigma = 1, mu = 0, ntimes = 200, nspecies = 10) {
  
  if (length(rho) > 1) {
    corr.mat=matrix(NA, nrow=nspecies, ncol=nspecies)
    corr.mat[upper.tri(corr.mat)]=rho
    corr.mat[lower.tri(corr.mat)]=corr.mat[upper.tri(corr.mat)]
  }
  else {
    corr.mat=matrix(rho, nrow=nspecies, ncol=nspecies)
  }
  diag(corr.mat)=1
  # Cholesky decomposition
  L=try(chol(corr.mat), silent=TRUE)
  if (class(L)!="try-error") {
    community=matrix(rnorm(ntimes*nspecies, sd=1, mean=0), nrow=ntimes, ncol=nspecies) %*% L
    # community=scale(community, center=TRUE, scale=TRUE)*sigma+mu
    community=scale(community, center=TRUE, scale=TRUE)*sigma+mu
    attr(community, "scaled:center")=NULL
    attr(community, "scaled:scale")=NULL
  }
  else {
    community=NA
    warning("Unable to generate desired correlation matrix (leading minor is not positive definite)")
  }
  
  results=list(rho=rho, sigma=sigma, mu=mu, community=community)
  class(results)="cormat"
  return (results)
}


## -------------------------------------------- ## 
##  First 4 scenarios                           ## 
##  Two sites - two species - 15 years          ##
## -------------------------------------------- ## 
# We will manually specify species relative frequencies 
# and local site total aggregate metic using temporal trends 
# this facilitate visualization
# s = 2 sites, and t = 15 years
s = 2; t = 15

#### CASE 1 - Low GammaComp - Low GammaAgg ####
## Step 1: computing species relative biomass in site 1
Y1.rel <- cbind(seq(0.25, 0.75, length.out = t),
                seq(0.75, 0.25, length.out = t))

## Step 2: computing species relative biomass in site 2, mirrored of site 1  
Y2.rel <- Y1.rel[,c(2,1)]

## Step 3: computing total site biomass
community <- cbind(seq(10, 17, length.out = t),
                   seq(17, 10, length.out = t))

## Step 4: retrieving species biomass
Y1.case1 <- community[,1] * Y1.rel
Y2.case1 <- community[,2] * Y2.rel

## aggregating data
Yreg <- Y1.case1 + Y2.case1
Yplot <- melt(rbind(
    cbind(as.data.frame(Y1.case1), time=seq(1,t,1), site=rep("Site 1", t)), 
    cbind(as.data.frame(Y2.case1), time=seq(1,t,1), site=rep("Site 2", t)),
    cbind(as.data.frame(Yreg), time=seq(1,t,1), site=rep("Region", t))), id.vars = c("time", "site"))

## plot communities
ggplot(Yplot, aes(x = time, y = value, fill = variable)) + 
  geom_area(stat = "identity") +
  facet_wrap( ~ site) +
  scale_fill_manual(values=c("steelblue2", "coral2"), name="Species") +
  xlab("Time") + ylab("Biomass/Abundance") +
  ggtitle("") +
  theme_bw()

## Compute metacom stability
aggregate_stab(rbind(Y1.case1, Y2.case1), s, t)
compo_stab(rbind(Y1.case1, Y2.case1), s, t)
compo_stab_rel(rbind(Y1.case1, Y2.case1), s, t)

#### CASE 2 - Low GammaComp - High GammaAgg ####
## Step 1: computing species relative biomass in site 1
Y1.rel <- cbind(seq(0.25, 0.75, length.out = t),
                seq(0.75, 0.25, length.out = t))

## Step 2: computing species relative biomass in site 2, mirrored of site 1  
Y2.rel <- Y1.rel[,c(2,1)]

## Step 3: computing total site biomass
community <- cbind(seq(10, 17, length.out = t),
                   seq(10, 17, length.out = t))

## Step 4: retrieving species biomass
Y1.case2 <- community[,1] * Y1.rel
Y2.case2 <- community[,2] * Y2.rel

## aggregating data
Yreg <- Y1.case2 + Y2.case2
Yplot <- melt(rbind(
    cbind(as.data.frame(Y1.case2), time=seq(1,t,1), site=rep("Site 1", t)), 
    cbind(as.data.frame(Y2.case2), time=seq(1,t,1), site=rep("Site 2", t)),
    cbind(as.data.frame(Yreg), time=seq(1,t,1), site=rep("Region", t))), id.vars = c("time", "site"))

## plot communities
ggplot(Yplot, aes(x = time, y = value, fill = variable)) + 
  geom_area(stat = "identity") +
  facet_wrap( ~ site) +
  scale_fill_manual(values=c("steelblue2", "coral2"), name="Species") +
  xlab("Time") + ylab("Biomass/Abundance") +
  ggtitle("") +
  theme_bw()

## Compute metacom stability
aggregate_stab(rbind(Y1.case2, Y2.case2), s, t)
compo_stab(rbind(Y1.case2, Y2.case2), s, t)
compo_stab_rel(rbind(Y1.case2, Y2.case2), s, t)

#### CASE 3 - High GammaComp - Low GammaAgg ####
## Step 1: computing species relative biomass in site 1
Y1.rel <- cbind(seq(0.25, 0.75, length.out = t),
                seq(0.75, 0.25, length.out = t))

## Step 2: computing species relative biomass in site 2, same as in site 1  
Y2.rel <- Y1.rel

## Step 3: computing total site biomass
community <- cbind(seq(10, 17, length.out = t),
                   seq(17, 10, length.out = t))

## Step 4: retrieving species biomass
Y1.case3 <- community[,1] * Y1.rel
Y2.case3 <- community[,2] * Y2.rel

## aggregating data
Yreg <- Y1.case3 + Y2.case3
Yplot <- melt(rbind(
    cbind(as.data.frame(Y1.case3), time=seq(1,t,1), site=rep("Site 1", t)), 
    cbind(as.data.frame(Y2.case3), time=seq(1,t,1), site=rep("Site 2", t)),
    cbind(as.data.frame(Yreg), time=seq(1,t,1), site=rep("Region", t))), id.vars = c("time", "site"))

## plot communities
ggplot(Yplot, aes(x = time, y = value, fill = variable)) + 
  geom_area(stat = "identity") +
  facet_wrap( ~ site) +
  scale_fill_manual(values=c("steelblue2", "coral2"), name="Species") +
  xlab("Time") + ylab("Biomass/Abundance") +
  ggtitle("") +
  theme_bw()

## Compute metacom stability
aggregate_stab(rbind(Y1.case3, Y2.case3), s, t)
compo_stab(rbind(Y1.case3, Y2.case3), s, t)
compo_stab_rel(rbind(Y1.case3, Y2.case3), s, t)

#### CASE 4 - High GammaComp - High GammaAgg ####
## Step 1: computing species relative biomass in site 1
Y1.rel <- cbind(seq(0.25, 0.75, length.out = t),
                seq(0.75, 0.25, length.out = t))

## Step 2: computing species relative biomass in site 2, same as in site 1
Y2.rel <- Y1.rel

## Step 3: computing total site biomass
community <- cbind(seq(10, 17, length.out = t),
                   seq(10, 17, length.out = t))

## Step 4: retrieving species biomass
Y1.case4 <- community[,1] * Y1.rel
Y2.case4 <- community[,2] * Y2.rel

## aggregating data
Yreg <- Y1.case4 + Y2.case4
Yplot <- melt(rbind(
    cbind(as.data.frame(Y1.case4), time=seq(1,t,1), site=rep("Site 1", t)), 
    cbind(as.data.frame(Y2.case4), time=seq(1,t,1), site=rep("Site 2", t)),
    cbind(as.data.frame(Yreg), time=seq(1,t,1), site=rep("Region", t))), id.vars = c("time", "site"))

## plot communities
ggplot(Yplot, aes(x = time, y = value, fill = variable)) + 
  geom_area(stat = "identity") +
  facet_wrap( ~ site) +
  scale_fill_manual(values=c("steelblue2", "coral2"), name="Species") +
  xlab("Time") + ylab("Biomass/Abundance") +
  ggtitle("") +
  theme_bw()

## Compute metacom stability
aggregate_stab(rbind(Y1.case4, Y2.case4), s, t)
compo_stab(rbind(Y1.case4, Y2.case4), s, t)
compo_stab_rel(rbind(Y1.case4, Y2.case4), s, t)


## -------------------------------------------- ## 
##  Exploring paradox of case 3                 ## 
##  higher GammaComp than AlphaComp             ##
## -------------------------------------------- ## 
BD1 <- beta.div.comp(Y1.case3, coef="S", quant=TRUE) 
BD2 <- beta.div.comp(Y2.case3, coef="S", quant=TRUE) 
BDM <- beta.div.comp(Y1.case3+Y2.case3, coef="S", quant=TRUE) 
BD1$part; BD2$part; BDM$part

BD1$D
BD2$D
BDM$D
sum(BDM$D)/(t*(t-1))
sum(BD1$D)/(t*(t-1))
sum(BD2$D)/(t*(t-1))

## Different BD values
## using relative frequencies
BD1 <- beta.div.comp(Y1.case3/apply(Y1.case3, 1, sum), coef="S", quant=TRUE) 
BD2 <- beta.div.comp(Y2.case3/apply(Y2.case3, 1, sum), coef="S", quant=TRUE) 
BDM <- beta.div.comp((Y1.case3+Y2.case3)/apply((Y1.case3+Y2.case3), 1, sum), coef="S", quant=TRUE) 
BD1$part; BD2$part; BDM$part


## checking with another false dataset
Y1.rel <- Y2.rel <- cbind(seq(0.20, 0.80, length.out = 4),
                          seq(0.80, 0.20, length.out = 4))

community <- cbind(seq(10, 40, length.out = 4),
                   seq(40, 10, length.out = 4))

Y1.case3 <- community[,1] * Y1.rel
Y2.case3 <- community[,2] * Y2.rel

## aggregating data
Yreg <- Y1.case3 + Y2.case3
Yplot <- melt(rbind(
    cbind(as.data.frame(Y1.case3), time=seq(1,4,1), site=rep("Site 1", 4)), 
    cbind(as.data.frame(Y2.case3), time=seq(1,4,1), site=rep("Site 2", 4)),
    cbind(as.data.frame(Yreg), time=seq(1,4,1), site=rep("Region", 4))), id.vars = c("time", "site"))

## plot communities
ggplot(Yplot, aes(x = time, y = value, fill = variable)) + 
  geom_area(stat = "identity") +
  facet_wrap( ~ site) +
  scale_fill_manual(values=c("steelblue2", "coral2"), name="Species") +
  xlab("Time") + ylab("Biomass/Abundance") +
  ggtitle("") +
  theme_bw()

BD1 <- beta.div.comp(Y1.case3, coef="S", quant=TRUE) 
BD2 <- beta.div.comp(Y2.case3, coef="S", quant=TRUE) 
BDM <- beta.div.comp(Yreg, coef="S", quant=TRUE) 
BD1$part; BD2$part; BDM$part

BD1$D
BD2$D
BDM$D
n = 4
sum(BDM$D)/(n*(n-1))
sum(BD1$D)/(n*(n-1))
sum(BD2$D)/(n*(n-1))

## with relative frequencies
BD1 <- beta.div.comp(Y1.rel, coef="S", quant=TRUE) 
BD2 <- beta.div.comp(Y2.rel, coef="S", quant=TRUE) 
BDM <- beta.div.comp(Yreg/apply(Yreg, 1, sum), coef="S", quant=TRUE) 
BD1$part; BD2$part; BDM$part


## -------------------------------------------- ## 
##  Second 4 scenarios without temporal trends  ## 
##  Two sites - two species - 15 years          ##
## -------------------------------------------- ## 
s = 2; t = 15
set.seed(123)

#### CASE 1 - Low GammaComp - Low GammaAgg ####
## Site 1: species relative frequencies are positively correlated 0.95
## Site 2: species relative frequencies is the mirror of site 1 (species 1 <-> species 2)
## Species 1 is 3 times more abundant than species 2 in site 1
## Species 2 is 3 times more abundant than species 1 in site 2
## Strong spatial asynchrony in total aggregate community biomass between site 1 and 2

## Step 1: computing species relative biomass
Y1.rel <- correlated.matrix(rho = 0.95, sigma = 5, mu = 10, ntimes = 15, nspecies = 2)$community
## make species 1 three time more abundant
Y1.rel[Y1.rel < 0] <- 0
Y1.rel[,1] <- Y1.rel[,1] * 3
Y1.rel <- Y1.rel/apply(Y1.rel, 1, sum)
Y2.rel <- Y1.rel[,c(2,1)]

## step 2: set spatial correlation among total biomass
corr.mat = matrix(-0.95, nrow=2, ncol=2); diag(corr.mat) = 1
L = try(chol(corr.mat), silent=TRUE)
  if (class(L)!="try-error") {
    community=matrix(rnorm(15*2, sd=1, mean=0), nrow=15, ncol=2) %*% L
    # community=scale(community, center=TRUE, scale=TRUE)*sigma+mu
    community[,1] <- community[,1] * 3 + 10
    community[,2] <- community[,2] * 3 + 10 }

## step 3: retrieve species biomass
Y1.case1 <- community[,1] * Y1.rel
Y2.case1 <- community[,2] * Y2.rel
Yreg <- Y1.case1 + Y2.case1

## aggregating data
Yplot <- melt(rbind(
    cbind(as.data.frame(Y1.case1), time=seq(1,t,1), site=rep("Site 1", t)), 
    cbind(as.data.frame(Y2.case1), time=seq(1,t,1), site=rep("Site 2", t)),
    cbind(as.data.frame(Yreg), time=seq(1,t,1), site=rep("Region", t))), id.vars = c("time", "site"))
levels(Yplot$variable) <- c("Species 1", "Species 2") 

## plot communities
ggplot(Yplot, aes(x = time, y = value, fill = variable)) + 
  geom_area(stat = "identity") +
  facet_wrap( ~ site) +
  scale_fill_manual(values=c("steelblue2", "coral2"), name="Species") +
  xlab("Time") + ylab("Biomass/Abundance") +
  ggtitle("") +
  theme_bw()

## Compute metacom stability
aggregate_stab(rbind(Y1.case1, Y2.case1), s, t)
compo_stab(rbind(Y1.case1, Y2.case1), s, t)


#### CASE 2 - Low GammaComp - High GammaAgg
## Site 1: species relative frequencies are positively correlated 0.95
## Site 2: species relative frequencies is the mirror of site 1 (species 1 <-> species 2)
## Species 1 is 3 times more abundant than species 2 in site 1
## Species 2 is 3 times more abundant than species 1 in site 2
## Low spatial asynchrony in total aggregate community biomass between site 1 and 2

## Step 1: computing species relative biomass
Y1.rel <- correlated.matrix(rho = 0.95, sigma = 5, mu = 10, ntimes = 15, nspecies = 2)$community
## make species 1 three time more abundant
Y1.rel[Y1.rel < 0] <- 0
Y1.rel[,1] <- Y1.rel[,1] * 3
Y1.rel <- Y1.rel/apply(Y1.rel, 1, sum)
Y2.rel <- Y1.rel[,c(2,1)]

## step 2: set spatial correlation among total biomass
corr.mat = matrix(0.95, nrow=2, ncol=2); diag(corr.mat) = 1
L = try(chol(corr.mat), silent=TRUE)
  if (class(L)!="try-error") {
    # community=matrix(apply(Y1.case1, 1, sum), nrow=15, ncol=2) %*% L
    community=matrix(rnorm(15*2, sd=1, mean=0), nrow=15, ncol=2) %*% L
    # community=scale(community, center=TRUE, scale=TRUE)*sigma+mu
    # community=scale(community, center=TRUE, scale=TRUE)*sigma+mu
    community[,1] <- community[,1] * 3 + 10
    community[,2] <- community[,2] * 3 + 10 }

## step 3: retrieve species biomass
Y1.case2 <- community[,1] * Y1.rel
Y2.case2 <- community[,2] * Y2.rel
Yreg <- Y1.case2 + Y2.case2

## aggregating data
Yplot <- melt(rbind(
    cbind(as.data.frame(Y1.case2), time=seq(1,t,1), site=rep("Site 1", t)), 
    cbind(as.data.frame(Y2.case2), time=seq(1,t,1), site=rep("Site 2", t)),
    cbind(as.data.frame(Yreg), time=seq(1,t,1), site=rep("Region", t))), id.vars = c("time", "site"))
levels(Yplot$variable) <- c("Species 1", "Species 2") 

## plot communities
ggplot(Yplot, aes(x = time, y = value, fill = variable)) + 
  geom_area(stat = "identity") +
  facet_wrap( ~ site) +
  scale_fill_manual(values=c("steelblue2", "coral2"), name="Species") +
  xlab("Time") + ylab("Biomass/Abundance") +
  ggtitle("") +
  theme_bw()

## Compute metacom stability
aggregate_stab(rbind(Y1.case2, Y2.case2), s, t)
compo_stab(rbind(Y1.case2, Y2.case2), s, t)


#### CASE 3 - High GammaComp - Low GammaAgg
## Site 1: species relative frequencies are negatively correlated -0.95
## Site 2: species relative frequencies is the same as in site 1
## Species 1 is 3 times more abundant than species 2 in site 1
## Species 1 is 3 times more abundant than species 2 in site 2
## High spatial asynchrony in total aggregate community biomass between site 1 and 2

## Step 1: computing species relative biomass
Y1.rel <- correlated.matrix(rho = -0.95, sigma = 5, mu = 10, ntimes = 15, nspecies = 2)$community
## make species 1 three time more abundant
Y1.rel[Y1.rel < 0] <- 0
Y1.rel[,1] <- Y1.rel[,1] * 3
Y1.rel <- Y1.rel/apply(Y1.rel, 1, sum)
Y2.rel <- Y1.rel

## step 2: set spatial correlation among total biomass
corr.mat = matrix(-0.95, nrow=2, ncol=2); diag(corr.mat) = 1
L = try(chol(corr.mat), silent=TRUE)
  if (class(L)!="try-error") {
    # community=matrix(apply(Y1.case1, 1, sum), nrow=15, ncol=2) %*% L
    community=matrix(rnorm(15*2, sd=1, mean=0), nrow=15, ncol=2) %*% L
    # community=scale(community, center=TRUE, scale=TRUE)*sigma+mu
    # community=scale(community, center=TRUE, scale=TRUE)*sigma+mu
    community[,1] <- community[,1] * 3 + 10
    community[,2] <- community[,2] * 3 + 10 }

## step 3: retrieve species biomass
Y1.case3 <- community[,1] * Y1.rel
Y2.case3 <- community[,2] * Y2.rel
Yreg <- Y1.case3 + Y2.case3

## aggregating data
Yplot <- melt(rbind(
    cbind(as.data.frame(Y1.case3), time=seq(1,t,1), site=rep("Site 1", t)), 
    cbind(as.data.frame(Y2.case3), time=seq(1,t,1), site=rep("Site 2", t)),
    cbind(as.data.frame(Yreg), time=seq(1,t,1), site=rep("Region", t))), id.vars = c("time", "site"))
levels(Yplot$variable) <- c("Species 1", "Species 2") 

## plot communities
ggplot(Yplot, aes(x = time, y = value, fill = variable)) + 
  geom_area(stat = "identity") +
  facet_wrap( ~ site) +
  scale_fill_manual(values=c("steelblue2", "coral2"), name="Species") +
  xlab("Time") + ylab("Biomass/Abundance") +
  ggtitle("") +
  theme_bw()

## Compute metacom stability
aggregate_stab(rbind(Y1.case3, Y2.case3), s, t)
compo_stab(rbind(Y1.case3, Y2.case3), s, t)

#### CASE 4 - High GammaComp - High GammaAgg
## Site 1: species relative frequencies are negatively correlated -0.95
## Site 2: species relative frequencies is the same as in site 1
## Species 1 is 3 times more abundant than species 2 in site 1
## Species 1 is 3 times more abundant than species 2 in site 2
## Low spatial asynchrony in total aggregate community biomass between site 1 and 2

## Step 1: computing species relative biomass
Y1.rel <- correlated.matrix(rho = -0.95, sigma = 5, mu = 10, ntimes = 15, nspecies = 2)$community
## make species 1 three time more abundant
Y1.rel[Y1.rel < 0] <- 0
Y1.rel[,1] <- Y1.rel[,1] * 3
Y1.rel <- Y1.rel/apply(Y1.rel, 1, sum)
Y2.rel <- Y1.rel

## step 2: set spatial correlation among total biomass
corr.mat = matrix(0.95, nrow=2, ncol=2); diag(corr.mat) = 1
L = try(chol(corr.mat), silent=TRUE)
  if (class(L)!="try-error") {
    # community=matrix(apply(Y1.case1, 1, sum), nrow=15, ncol=2) %*% L
    community=matrix(rnorm(15*2, sd=1, mean=0), nrow=15, ncol=2) %*% L
    # community=scale(community, center=TRUE, scale=TRUE)*sigma+mu
    # community=scale(community, center=TRUE, scale=TRUE)*sigma+mu
    community[,1] <- community[,1] * 3 + 10
    community[,2] <- community[,2] * 3 + 10 }

## step 3: retrieve species biomass
Y1.case4 <- community[,1] * Y1.rel
Y2.case4 <- community[,2] * Y2.rel
Yreg <- Y1.case4 + Y2.case4

## aggregating data
Yplot <- melt(rbind(
    cbind(as.data.frame(Y1.case4), time=seq(1,t,1), site=rep("Site 1", t)), 
    cbind(as.data.frame(Y2.case4), time=seq(1,t,1), site=rep("Site 2", t)),
    cbind(as.data.frame(Yreg), time=seq(1,t,1), site=rep("Region", t))), id.vars = c("time", "site"))
levels(Yplot$variable) <- c("Species 1", "Species 2") 

## plot communities
ggplot(Yplot, aes(x = time, y = value, fill = variable)) + 
  geom_area(stat = "identity") +
  facet_wrap( ~ site) +
  scale_fill_manual(values=c("steelblue2", "coral2"), name="Species") +
  xlab("Time") + ylab("Biomass/Abundance") +
  ggtitle("") +
  theme_bw()

## Compute metacom stability
aggregate_stab(rbind(Y1.case4, Y2.case4), s, t)
compo_stab(rbind(Y1.case4, Y2.case4), s, t)



## -------------------------------------------- ## 
##  Other scenarios                             ## 
##  Two sites - two species - 15 years          ##
## -------------------------------------------- ## 
# We will only specify correlations between:
  # species relative frequencies in each sites
  # total aggregate community metric between the two sites
s = 2
t = 15
set.seed(123)

#### CASE 1 - Low GammaComp variability - Low/medium GammaAgg variability ####
## Site 1 has 2 species with a correlation of -0.95, μ = 9 and σ = 5
## Site 2 is the mirror of site 1 (we exchange species idendity)
## Hence same local aggregate and compositional variability
## but the compositional variability has spatial asynchrony

## site 1
Y1.case1 <- correlated.matrix(rho = -0.95, sigma = 5, mu = 9, ntimes = 15, nspecies = 2)$community
Y1.case1[Y1.case1 < 0] <- 0
## site 2: reverse species dynamics
Y2.case1 <- Y1.case1[,c(2,1)]
## regional scale
Yreg.case1 <- Y1.case1 + Y2.case1

## aggregating local and regional data
Yplot.case1 <- melt(rbind(
    cbind(as.data.frame(Y1.case1), time=seq(1,t,1), site=rep("Site 1", t)), 
    cbind(as.data.frame(Y2.case1), time=seq(1,t,1), site=rep("Site 2", t)),
    cbind(as.data.frame(Yreg.case1), time=seq(1,t,1), site=rep("Region", t))),
      id.vars = c("time", "site"))
levels(Yplot.case1$variable) <- c("Species 1", "Species 2") 

## plot communities
ggplot(Yplot.case1, aes(x = time, y = value, fill = variable)) + 
  geom_area(stat = "identity") +
  facet_wrap( ~ site) +
  scale_fill_manual(values=c("steelblue2", "coral2"), name="Species") +
  xlab("Time") + ylab("Biomass/Abundance") +
  ggtitle("") +
  theme_bw()

## Compute metacom stability
aggregate_stab(rbind(Y1.case1, Y2.case1), s, t)
compo_stab(rbind(Y1.case1, Y2.case1), s, t)

#### CASE 2 - ####
## we use the same species relative frequencies in site 1 and 2 from CASE 1
## we impose aggregate community metric to be the same in both sites
Y1.case2 <- Y1.case1/apply(Y1.case1, 1, sum)
Y2.case2 <- Y2.case1/apply(Y2.case1, 1, sum)

Y1.case2 <- Y1.case2 * rnorm(15, mean = mean(apply(Y1.case1, 1, sum)), sd = sd(apply(Y1.case1, 1, sum)) * 3)
Y2.case2 <- Y2.case2 * rnorm(15, mean = mean(apply(Y2.case1, 1, sum)), sd = sd(apply(Y2.case1, 1, sum)) * 3)
## regional scale
Yreg.case2 <- Y1.case2 + Y2.case2

## aggregating data
Yplot.case2 <- melt(rbind(
    cbind(as.data.frame(Y1.case2), time=seq(1,t,1), site=rep("Site 1", t)), 
    cbind(as.data.frame(Y2.case2), time=seq(1,t,1), site=rep("Site 2", t)),
    cbind(as.data.frame(Yreg.case2), time=seq(1,t,1), site=rep("Region", t))), id.vars = c("time", "site"))
levels(Yplot.case2$variable) <- c("Species 1", "Species 2") 

## plot communities
ggplot(Yplot.case2, aes(x = time, y = value, fill = variable)) + 
  geom_area(stat = "identity") +
  facet_wrap( ~ site) +
  scale_fill_manual(values=c("steelblue2", "coral2"), name="Species") +
  xlab("Time") + ylab("Biomass/Abundance") +
  ggtitle("") +
  theme_bw()

## Compute metacom stability
aggregate_stab(rbind(Y1.case2, Y2.case2), s, t)
compo_stab(rbind(Y1.case2, Y2.case2), s, t)

#### CASE 3 - imposing strong asynchrony in aggregate community metric between site 1 and 2 ####
## species relative frequencies in site 1 and site 2 are mirrored
## Step 1: computing species relative biomass
Y1.rel <- correlated.matrix(rho = -0.95, sigma = 5, mu = 10, ntimes = 15, nspecies = 2)$community
Y1.rel[Y1.rel < 0] <- 0
Y1.rel <- Y1.rel/apply(Y1.rel, 1, sum)
Y2.rel <- Y1.rel[,c(2,1)]

## step 2: set spatial correlation among total biomass
corr.mat = matrix(-0.95, nrow=2, ncol=2); diag(corr.mat) = 1
L = try(chol(corr.mat), silent=TRUE)
  if (class(L)!="try-error") {
    community=matrix(rnorm(15*2, sd=1, mean=0), nrow=15, ncol=2) %*% L
    # community=scale(community, center=TRUE, scale=TRUE)*sigma+mu
    community[,1] <- community[,1] * 3 + 10
    community[,2] <- community[,2] * 3 + 10 }

Y1.case3 <- community[,1] * Y1.rel
Y2.case3 <- community[,2] * Y2.rel
Yreg <- Y1.case3 + Y2.case3

## aggregating data
Yplot <- melt(rbind(
    cbind(as.data.frame(Y1.case3), time=seq(1,t,1), site=rep("Site 1", t)), 
    cbind(as.data.frame(Y2.case3), time=seq(1,t,1), site=rep("Site 2", t)),
    cbind(as.data.frame(Yreg), time=seq(1,t,1), site=rep("Region", t))), id.vars = c("time", "site"))

## plot communities
ggplot(Yplot, aes(x = time, y = value, fill = variable)) + 
  geom_area(stat = "identity") +
  facet_wrap( ~ site) +
  scale_fill_manual(values=c("steelblue2", "coral2"), name="Species") +
  xlab("Time") + ylab("Biomass/Abundance") +
  ggtitle("") +
  theme_bw()

## Compute metacom stability
aggregate_stab(rbind(Y1.case3, Y2.case3), s, t)
compo_stab(rbind(Y1.case3, Y2.case3), s, t)

#### CASE 4 - imposing strong synchrony in aggregate community metric between site 1 and 2 ####
## species relative frequencies in site 1 and site 2 are mirrored

## Creating simple case study with two sites and two species
## Step 1: computing species relative biomass
Y1.rel <- correlated.matrix(rho = -0.95, sigma = 5, mu = 10, ntimes = 15, nspecies = 2)$community
Y1.rel[Y1.rel < 0] <- 0
Y1.rel <- Y1.rel/apply(Y1.rel, 1, sum)
Y2.rel <- Y1.rel[,c(2,1)]

## step 2: set spatial correlation among total biomass
corr.mat = matrix(0.95, nrow=2, ncol=2); diag(corr.mat) = 1
L = try(chol(corr.mat), silent=TRUE)
  if (class(L)!="try-error") {
    community=matrix(rnorm(15*2, sd=1, mean=0), nrow=15, ncol=2) %*% L
    # community=scale(community, center=TRUE, scale=TRUE)*sigma+mu
    community[,1] <- community[,1] * 3 + 10
    community[,2] <- community[,2] * 3 + 10 }

Y1.case4 <- community[,1] * Y1.rel
Y2.case4 <- community[,2] * Y2.rel
Yreg <- Y1.case4 + Y2.case4

## aggregating data
Yplot <- melt(rbind(
    cbind(as.data.frame(Y1.case4), time=seq(1,15,1), site=rep("Site 1", 15)), 
    cbind(as.data.frame(Y2.case4), time=seq(1,15,1), site=rep("Site 2", 15)),
    cbind(as.data.frame(Yreg), time=seq(1,15,1), site=rep("Region", 15))), id.vars = c("time", "site"))

## plot communities
ggplot(Yplot, aes(x = time, y = value, fill = variable)) + 
  geom_area(stat = "identity") +
  facet_wrap( ~ site) +
  scale_fill_manual(values=c("steelblue2", "coral2"), name="Species") +
  xlab("Time") + ylab("Biomass/Abundance") +
  ggtitle("") +
  theme_bw()

## Compute metacom stability
aggregate_stab(rbind(Y1.case4, Y2.case4), s, t)
compo_stab(rbind(Y1.case4, Y2.case4), s, t)



