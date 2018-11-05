#################################################################################### 
## Numerical examples illustrating the dual nature of metacommunity stability     ##
## Update version with temporal fluctuations (instead of trends before)           ##
#################################################################################### 
## Thomas Lamy, March 25th 2017

#### setting R ####
## load packages
library(reshape2)
library(ggplot2)
## load function space_stab to compute compositional and aggregate stability across spatial scales
source("~/Library/Mobile Documents/com~apple~CloudDocs/Analysis/2018_metacom_stab/space_stab.R")
## set the working directory 
my_wd <- "~/Library/Mobile Documents/com~apple~CloudDocs/Analysis/2018_metacom_stab/"
## function correlated.matrix from the synchrony package (Tarik C. Gouhier)
## Create an n times x nspecies matrix with correlation ρ, standard deviation σ and mean μ
correlated.matrix <- function (rho = 0, sigma = 1, mu = 0, ntimes = 200, nspecies = 10){
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

## Simple examples with two sites (s); two species; 15 years (t)
set.seed(123)
s = 2; t = 15

#### CASE 1 - Low compositional metacom variability - Low aggregate metacom variability ####
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
corr.mat <- matrix(-0.95, nrow=2, ncol=2); diag(corr.mat) = 1
L <- try(chol(corr.mat), silent=TRUE)
if(class(L)!="try-error"){
  community=matrix(rnorm(15*2, sd=1, mean=0), nrow=15, ncol=2) %*% L
  # community=scale(community, center=TRUE, scale=TRUE)*sigma+mu
  community[,1] <- community[,1] * 3 + 10
  community[,2] <- community[,2] * 3 + 10 }

## step 3: retrieve species biomass
Y1.case1 <- community[,1] * Y1.rel
Y2.case1 <- community[,2] * Y2.rel
Yreg <- Y1.case1 + Y2.case1
Y <- rbind(Y1.case1, Y2.case1)

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
  xlab("Time") + ylab("Biomass/Abundance") + ggtitle("") + theme_bw() 

## Compute compositional and aggregate stability
space_stab(rbind(Y1.case1, Y2.case1), s, t)
# GammaCV    AlphaCV      PhiCV     GammaTBD   AlphaTBD    PhiTBD    GammaHBD   AlphaHBD    PhiHBD  
# 0.001406513 0.06947463 0.02024499 0.0003362415 0.00259759 0.1294436 0.004647802 0.02116499 0.2195986 

## Plot type 2 
## merging data
Y1.case1 <- cbind(Y1.case1, apply(Y1.case1,1,sum))
Y2.case1 <- cbind(Y2.case1, apply(Y2.case1,1,sum))
Yreg <- Y1.case1 + Y2.case1
Yplot <- melt(rbind(
  cbind(as.data.frame(Y1.case1), time=seq(1,t,1), site=rep("Site 1", t)), 
  cbind(as.data.frame(Y2.case1), time=seq(1,t,1), site=rep("Site 2", t)),
  cbind(as.data.frame(Yreg), time=seq(1,t,1), site=rep("Region", t))), id.vars = c("time", "site"))
levels(Yplot$variable) <- c("Species 1", "Species 2", "Total")
## plot communities
setwd(paste(my_wd, "Figures", sep=""))
pdf("Figure1_case1_fluctuations.pdf", width=8, height=3)
ggplot(Yplot, aes(x=time, y=value, fill=variable)) + 
  geom_line(aes(linetype=variable, color=variable, size=variable)) +
  facet_wrap(~site) +
  scale_linetype_manual(values=c("dashed", "dotted", "solid")) +
  scale_color_manual(values=c("steelblue2", "coral2", "black")) +
  scale_size_manual(values=c(1,1,2)) +
  xlab("Time") + ylab("Biomass") + ggtitle("") + theme_bw() + theme(legend.position="none")
dev.off()

## Plot type 3 
Yplot.region <- droplevels(subset(Yplot, variable=="Total"))
Yplot.sp1 <- droplevels(subset(Yplot, variable=="Species 1"))
Yplot.sp2 <- droplevels(subset(Yplot, variable=="Species 2"))
Yplot2 <- cbind(Yplot.region, sp1=Yplot.sp1$value, sp2=Yplot.sp2$value)
levels(Yplot2$site) <- c("Community 1", "Community 2", "Metacommunity") 
## plot communities
setwd(paste(my_wd, "Figures", sep=""))
pdf("Figure1_case1_fluctuations2.pdf", width=8, height=3)
ggplot(Yplot2, aes(x=time)) + 
  geom_line(aes(y=value), linetype="solid", color="darkgrey", size=2, alpha=0.8) +
  geom_line(aes(y=sp1), linetype="dashed", color="steelblue2", size=1, alpha=1) +
  geom_line(aes(y=sp2), linetype="dotted", color="coral2", size=1, alpha=1) +
  facet_wrap(~site) +
  xlab("Time") + ylab("Biomass") + ggtitle("") + theme_bw() + theme(legend.position="none")
dev.off()

#### CASE 2 - Low compositional metacom variability - High aggregate metacom variability ####
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
corr.mat <- matrix(0.95, nrow=2, ncol=2); diag(corr.mat) = 1
L <- try(chol(corr.mat), silent=TRUE)
if (class(L)!="try-error") {
# community=matrix(apply(Y1.case1, 1, sum), nrow=15, ncol=2) %*% L
  community=matrix(rnorm(15*2, sd=1, mean=0), nrow=15, ncol=2) %*% L
  # community=scale(community, center=TRUE, scale=TRUE)*sigma+mu
  community[,1] <- community[,1] * 3 + 10
  community[,2] <- community[,2] * 3 + 10 }

## step 3: retrieve species biomass
Y1.case2 <- community[,1] * Y1.rel
Y2.case2 <- community[,2] * Y2.rel
Yreg <- Y1.case2 + Y2.case2
Y <- rbind(Y1.case2, Y2.case2)

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
  xlab("Time") + ylab("Biomass/Abundance") + ggtitle("") + theme_bw() 

## Compute compositional and aggregate stability
space_stab(rbind(Y1.case2, Y2.case2), s, t)
# GammaCV    AlphaCV     PhiCV    GammaTBD    AlphaTBD   PhiTBD     GammaHBD AlphaHBD      PhiHBD   
# 0.0813664 0.08260147 0.9850478 0.001274952 0.005861282 0.217521 8.924551e-05 0.067707 0.001318113

## Plot type 2 
## merging data
Y1.case2 <- cbind(Y1.case2, apply(Y1.case2,1,sum))
Y2.case2 <- cbind(Y2.case2, apply(Y2.case2,1,sum))
Yreg <- Y1.case2 + Y2.case2
Yplot <- melt(rbind(
  cbind(as.data.frame(Y1.case2), time=seq(1,t,1), site=rep("Site 1", t)), 
  cbind(as.data.frame(Y2.case2), time=seq(1,t,1), site=rep("Site 2", t)),
  cbind(as.data.frame(Yreg), time=seq(1,t,1), site=rep("Region", t))), id.vars = c("time", "site"))
levels(Yplot$variable) <- c("Species 1", "Species 2", "Total")
## plot communities
setwd(paste(my_wd, "Figures", sep=""))
pdf("Figure1_case2_fluctuations.pdf", width=8, height=3)
ggplot(Yplot, aes(x=time, y=value, fill=variable)) + 
  geom_line(aes(linetype=variable, color=variable, size=variable)) +
  facet_wrap(~site) +
  scale_linetype_manual(values=c("dashed", "dotted", "solid")) +
  scale_color_manual(values=c("steelblue2", "coral2", "black")) +
  scale_size_manual(values=c(1,1,2)) +
  xlab("Time") + ylab("Biomass") + ggtitle("") + theme_bw() + theme(legend.position="none")
dev.off()

## Plot type 3 
Yplot.region <- droplevels(subset(Yplot, variable=="Total"))
Yplot.sp1 <- droplevels(subset(Yplot, variable=="Species 1"))
Yplot.sp2 <- droplevels(subset(Yplot, variable=="Species 2"))
Yplot2 <- cbind(Yplot.region, sp1=Yplot.sp1$value, sp2=Yplot.sp2$value)
levels(Yplot2$site) <- c("Community 1", "Community 2", "Metacommunity") 
## plot communities
setwd(paste(my_wd, "Figures", sep=""))
pdf("Figure1_case2_fluctuations2.pdf", width=8, height=3)
ggplot(Yplot2, aes(x=time)) + 
  geom_line(aes(y=value), linetype="solid", color="darkgrey", size=2, alpha=0.8) +
  geom_line(aes(y=sp1), linetype="dashed", color="steelblue2", size=1, alpha=1) +
  geom_line(aes(y=sp2), linetype="dotted", color="coral2", size=1, alpha=1) +
  facet_wrap(~site) +
  xlab("Time") + ylab("Biomass") + ggtitle("") + theme_bw() + theme(legend.position="none")
dev.off()

#### CASE 3 - High compositional metacom variability - Low aggregate metacom variability ####
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
corr.mat <- matrix(-0.95, nrow=2, ncol=2); diag(corr.mat) = 1
L <- try(chol(corr.mat), silent=TRUE)
if (class(L)!="try-error") {
  # community=matrix(apply(Y1.case1, 1, sum), nrow=15, ncol=2) %*% L
  community=matrix(rnorm(15*2, sd=1, mean=0), nrow=15, ncol=2) %*% L
  # community=scale(community, center=TRUE, scale=TRUE)*sigma+mu
  community[,1] <- community[,1] * 3 + 10
  community[,2] <- community[,2] * 3 + 10 }

## step 3: retrieve species biomass
Y1.case3 <- community[,1] * Y1.rel
Y2.case3 <- community[,2] * Y2.rel
Yreg <- Y1.case3 + Y2.case3
Y <- rbind(Y1.case3, Y2.case3)

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
  xlab("Time") + ylab("Biomass/Abundance") + ggtitle("") + theme_bw() 

## Compute compositional and aggregate stability
space_stab(rbind(Y1.case3, Y2.case3), s, t)
# GammaCV   AlphaCV      PhiCV    GammaTBD    AlphaTBD    PhiTBD  GammaHBD  AlphaHBD PhiHBD 
# 0.001375791 0.1063021 0.01294227 0.007023634 0.009032708 0.7775779 0.1061049 0.1061049 

## Plot type 2 
## merging data
Y1.case3 <- cbind(Y1.case3, apply(Y1.case3,1,sum))
Y2.case3 <- cbind(Y2.case3, apply(Y2.case3,1,sum))
Yreg <- Y1.case3 + Y2.case3
Yplot <- melt(rbind(
  cbind(as.data.frame(Y1.case3), time=seq(1,t,1), site=rep("Site 1", t)), 
  cbind(as.data.frame(Y2.case3), time=seq(1,t,1), site=rep("Site 2", t)),
  cbind(as.data.frame(Yreg), time=seq(1,t,1), site=rep("Region", t))), id.vars = c("time", "site"))
levels(Yplot$variable) <- c("Species 1", "Species 2", "Total")
## plot communities
setwd(paste(my_wd, "Figures", sep=""))
pdf("Figure1_case3_fluctuations.pdf", width=8, height=3)
ggplot(Yplot, aes(x=time, y=value, fill=variable)) + 
  geom_line(aes(linetype=variable, color=variable, size=variable)) +
  facet_wrap(~site) +
  scale_linetype_manual(values=c("dashed", "dotted", "solid")) +
  scale_color_manual(values=c("steelblue2", "coral2", "black")) +
  scale_size_manual(values=c(1,1,2)) +
  xlab("Time") + ylab("Biomass") + ggtitle("") + theme_bw() + theme(legend.position="none")
dev.off()

## Plot type 3 
Yplot.region <- droplevels(subset(Yplot, variable=="Total"))
Yplot.sp1 <- droplevels(subset(Yplot, variable=="Species 1"))
Yplot.sp2 <- droplevels(subset(Yplot, variable=="Species 2"))
Yplot2 <- cbind(Yplot.region, sp1=Yplot.sp1$value, sp2=Yplot.sp2$value)
levels(Yplot2$site) <- c("Community 1", "Community 2", "Metacommunity") 
## plot communities
setwd(paste(my_wd, "Figures", sep=""))
pdf("Figure1_case3_fluctuations2.pdf", width=8, height=3)
ggplot(Yplot2, aes(x=time)) + 
  geom_line(aes(y=value), linetype="solid", color="darkgrey", size=2, alpha=0.8) +
  geom_line(aes(y=sp1), linetype="dashed", color="steelblue2", size=1, alpha=1) +
  geom_line(aes(y=sp2), linetype="dotted", color="coral2", size=1, alpha=1) +
  facet_wrap(~site) +
  xlab("Time") + ylab("Biomass") + ggtitle("") + theme_bw() + theme(legend.position="none")
dev.off()

#### CASE 4 - High compositional metacom variability - High aggregate metacom variability ####
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
corr.mat <- matrix(0.95, nrow=2, ncol=2); diag(corr.mat) = 1
L <- try(chol(corr.mat), silent=TRUE)
if(class(L)!="try-error") {
  # community=matrix(apply(Y1.case1, 1, sum), nrow=15, ncol=2) %*% L
  community=matrix(rnorm(15*2, sd=1, mean=0), nrow=15, ncol=2) %*% L
  # community=scale(community, center=TRUE, scale=TRUE)*sigma+mu
  community[,1] <- community[,1] * 3 + 10
  community[,2] <- community[,2] * 3 + 10 }

## step 3: retrieve species biomass
Y1.case4 <- community[,1] * Y1.rel
Y2.case4 <- community[,2] * Y2.rel
Yreg <- Y1.case4 + Y2.case4
Y <- rbind(Y1.case4, Y2.case4)

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
  xlab("Time") + ylab("Biomass/Abundance") + ggtitle("") + theme_bw() 

## Compute compositional and aggregate stability
space_stab(rbind(Y1.case4, Y2.case4), s, t)
# GammaCV   AlphaCV     PhiCV    GammaTBD    AlphaTBD    PhiTBD  GammaHBD  AlphaHBD PhiHBD
# 0.08980136 0.0908243 0.9887371 0.004774597 0.004795664 0.9956071 0.0512436 0.0512436      1

## Plot type 2 
## merging data
Y1.case4 <- cbind(Y1.case4, apply(Y1.case4,1,sum))
Y2.case4 <- cbind(Y2.case4, apply(Y2.case4,1,sum))
Yreg <- Y1.case4 + Y2.case4
Yplot <- melt(rbind(
  cbind(as.data.frame(Y1.case4), time=seq(1,t,1), site=rep("Site 1", t)), 
  cbind(as.data.frame(Y2.case4), time=seq(1,t,1), site=rep("Site 2", t)),
  cbind(as.data.frame(Yreg), time=seq(1,t,1), site=rep("Region", t))), id.vars = c("time", "site"))
levels(Yplot$variable) <- c("Species 1", "Species 2", "Total")
## plot communities
setwd(paste(my_wd, "Figures", sep=""))
pdf("Figure1_case4_fluctuations.pdf", width=8, height=3)
ggplot(Yplot, aes(x=time, y=value, fill=variable)) + 
  geom_line(aes(linetype=variable, color=variable, size=variable)) +
  facet_wrap(~site) +
  scale_linetype_manual(values=c("dashed", "dotted", "solid")) +
  scale_color_manual(values=c("steelblue2", "coral2", "black")) +
  scale_size_manual(values=c(1,1,2)) +
  xlab("Time") + ylab("Biomass") + ggtitle("") + theme_bw() + theme(legend.position="none")
dev.off()

## Plot type 3 
Yplot.region <- droplevels(subset(Yplot, variable=="Total"))
Yplot.sp1 <- droplevels(subset(Yplot, variable=="Species 1"))
Yplot.sp2 <- droplevels(subset(Yplot, variable=="Species 2"))
Yplot2 <- cbind(Yplot.region, sp1=Yplot.sp1$value, sp2=Yplot.sp2$value)
levels(Yplot2$site) <- c("Community 1", "Community 2", "Metacommunity") 
## plot communities
setwd(paste(my_wd, "Figures", sep=""))
pdf("Figure1_case3_fluctuations2.pdf", width=8, height=3)
ggplot(Yplot2, aes(x=time)) + 
  geom_line(aes(y=value), linetype="solid", color="darkgrey", size=2, alpha=0.8) +
  geom_line(aes(y=sp1), linetype="dashed", color="steelblue2", size=1, alpha=1) +
  geom_line(aes(y=sp2), linetype="dotted", color="coral2", size=1, alpha=1) +
  facet_wrap(~site) +
  xlab("Time") + ylab("Biomass") + ggtitle("") + theme_bw() + theme(legend.position="none")
dev.off()

## end

