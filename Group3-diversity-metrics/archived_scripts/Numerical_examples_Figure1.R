## Numerical examples illustrating the dual nature of metacommunity stability
## Thomas Lamy
## March 25th 2017

## load packages
library(reshape2)
library(ggplot2)

## load function space_stab to compute compositional and aggregate stability across spatial scales
source("~/Library/Mobile Documents/com~apple~CloudDocs/Analysis/2018_metacom_stab/space_stab.R")
## set the working directory 
my_wd <- "~/Library/Mobile Documents/com~apple~CloudDocs/Analysis/2018_metacom_stab/"

## Simple examples with two sites (s); two species; 15 years (t)
s = 2; t = 15

#### CASE 1 - Low compositional metacom variability - Low aggregate metacom variability ####
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

## merging data
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
  xlab("Time") + ylab("Biomass/Abundance") + ggtitle("") + theme_bw() 

## Compute compositional and aggregate stability
space_stab(rbind(Y1.case1, Y2.case1), s, t)

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
pdf("Figure1_case1.pdf", width=8, height=3)
ggplot(Yplot, aes(x=time, y=value, fill=variable)) + 
  geom_line(aes(linetype=variable, color=variable, size=variable)) +
  facet_wrap(~site) +
  scale_linetype_manual(values=c("dashed", "dotted", "solid")) +
  scale_color_manual(values=c("steelblue2", "coral2", "black")) +
  scale_size_manual(values=c(1,1,2)) +
  xlab("Time") + ylab("Biomass") + ggtitle("") + theme_bw() + theme(legend.position="none")
dev.off()

#### CASE 2 - Low compositional metacom variability - High aggregate metacom variability ####
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
  xlab("Time") + ylab("Biomass/Abundance") + ggtitle("") + theme_bw()

## Compute compositional and aggregate stability
space_stab(rbind(Y1.case2, Y2.case2), s, t)

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
pdf("Figure1_case2.pdf", width=8, height=3)
ggplot(Yplot, aes(x=time, y=value, fill=variable)) + 
  geom_line(aes(linetype=variable, color=variable, size=variable)) +
  facet_wrap(~site) +
  scale_linetype_manual(values=c("dashed", "dotted", "solid")) +
  scale_color_manual(values=c("steelblue2", "coral2", "black")) +
  scale_size_manual(values=c(1,1,2)) +
  xlab("Time") + ylab("Biomass") + ggtitle("") + theme_bw() + theme(legend.position="none")
dev.off()

#### CASE 3 - High compositional metacom variability - Low aggregate metacom variability ####
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
  xlab("Time") + ylab("Biomass/Abundance") + ggtitle("") + theme_bw()

## Compute compositional and aggregate stability
space_stab(rbind(Y1.case3, Y2.case3), s, t)

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
pdf("Figure1_case3.pdf", width=8, height=3)
ggplot(Yplot, aes(x=time, y=value, fill=variable)) + 
  geom_line(aes(linetype=variable, color=variable, size=variable)) +
  facet_wrap(~site) +
  scale_linetype_manual(values=c("dashed", "dotted", "solid")) +
  scale_color_manual(values=c("steelblue2", "coral2", "black")) +
  scale_size_manual(values=c(1,1,2)) +
  xlab("Time") + ylab("Biomass") + ggtitle("") + theme_bw() + theme(legend.position="none")
dev.off()

#### CASE 4 - High compositional metacom variability - High aggregate metacom variability ####
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
  xlab("Time") + ylab("Biomass/Abundance") + ggtitle("") + theme_bw()

## Compute compositional and aggregate stability
space_stab(rbind(Y1.case4, Y2.case4), s, t)

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
pdf("Figure1_case4.pdf", width=8, height=3)
ggplot(Yplot, aes(x=time, y=value, fill=variable)) + 
  geom_line(aes(linetype=variable, color=variable, size=variable)) +
  facet_wrap(~site) +
  scale_linetype_manual(values=c("dashed", "dotted", "solid")) +
  scale_color_manual(values=c("steelblue2", "coral2", "black")) +
  scale_size_manual(values=c(1,1,2)) +
  xlab("Time") + ylab("Biomass") + ggtitle("") + theme_bw() + theme(legend.position="none")
dev.off()

## end

