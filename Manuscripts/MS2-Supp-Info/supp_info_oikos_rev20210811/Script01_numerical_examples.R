#################################################################################### 
## Numerical examples based on mirrored communities                               ##
#################################################################################### 
## Thomas Lamy, June 20th 2021
## two mirrored communities are communities which relative frequencies change in opposite direction
## such as metacommunity compositional variability is null

#### setting R ####
## load packages
library(here)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggpubr)

## load function space_stab to compute compositional and aggregate variability across spatial scales
source(here("space_stab.R"))


#### Parameters ####
nbsp <- 4    # Number of species
t <- 15      # Number of time survey
s <- 2       # two mirrored communities
nbit <- 100  # Number of times Y2 will be stacked to Y1
## Total biomass is randomly draw from N(mu_com, sd_com)
mu_com <- 40; sd_com <- 8

#### Exploring the range of possible frequencies to create two mirrored communities ####
## We randomly generate species relative frequencies in Y1 and at the regional scale (Yreg)
## We then retrieve species relative frequencies in Y2 
## However, relative frequencies in Y2 are not always defined (can be < 0 or > 1)
## This section explore which relative frequencies can be used for Y1 and Yreg
F <- f <- seq(0, 1, 0.01)
test <- matrix(NA,101,101)
row.names(test) <- colnames(test) <- F
for(i in 1:101) for(j in 1:101) test[i,j] <- F[i]*2-f[j]   # formula to retrieve Y2
test.df <- melt(test, c("Yregfreq", "Y1freq"), value.name="Y2freq")
ggplot(test.df, aes(x=Yregfreq,y=Y1freq,fill=Y2freq)) +
  geom_tile() + scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=0)
## Y2 can display negative or > 1 frequencies
test.df2 <- test.df
test.df2[test.df2$Y2freq > 1,]$Y2freq <- "NA"
test.df2[test.df2$Y2freq < 0,]$Y2freq <- "NA"
test.df2$Y2freq <- as.numeric(test.df2$Y2freq)
ggplot(test.df2,aes(x=Yregfreq,y=Y1freq,fill=Y2freq)) +
  geom_tile() + scale_fill_gradient2(low="red", mid="white", high="blue", na.value="grey", midpoint=0)

#### Generate relative frequencies in Y1 and Y2 ####
## We randomly generate two mirrored community (Y1 & Y2) of 4 species (S1-4)
## First we define random but FIXED species relative frequency in the metacommunity
## FIXED species relative frequencies ensure that metacommunity compositional variability is NULL
## FIXED Species relative frequencies ranged from Fmin to Fmax
Fmin <- 0.25; Fmax <- 0.25
S1 <- rep(runif(1, Fmin, Fmax), t)
S2 <- rep(runif(1, Fmin, min(1-S1, Fmax)), t)
S3 <- rep(runif(1, Fmin, min(1-(S1+S2), Fmax)), t)
S4 <- 1-(S1+S2+S3)
Yreg <- matrix(c(S1, S2, S3, S4), nrow=t, ncol=4)
## Second we randomly generate species relative frequency in Y1
## Species relative frequencies range between fmin and fmax
fmin <- 0.1; fmax <- 0.4  # has to be between 0.1-0.4 otherwise negative relative frequencies can arise in Y2
## Function to generate two mirrored communities based on Yreg
mirrored.com <- function(Yreg){
  repeat{
    S1 <- runif(t, fmin, fmax)
    S2 <- runif(t, fmin, min(1-S1, fmax))
    S3 <- runif(t, fmin, min(1-(S1+S2), fmax))
    S4 <- 1-(S1+S2+S3)
    Y1 <- matrix(c(S1, S2, S3, S4), nrow=t, ncol=4)
    ## Third, we retrieve species relative frequency in Y2 following our formula
    ## our formula make sure Y1 & Y2 have mirrored compositional trajectories
    ## Since S4 is not bound to [fmin, fmax] (defined as the complement of the other species)
    ## We can still generate negative frequency for S4, this function make sure to return Y2 with no negative value
    Y2 <- (Yreg*2)-Y1
    if(sum(Y2 < 0) == 0) break  # leave the function only if the two communities are defined
  }
  xx <- list(Y1, Y2)
  return(xx)
}

#### Create inset ####
toto <- mirrored.com(Yreg)
Y1freq <- toto[[1]]
Y2freq <- toto[[2]]
Y1 <- Y1freq * rnorm(t, sd=sd_com, mean=mu_com)
Y2 <- Y2freq * rnorm(t, sd=sd_com, mean=mu_com)
Y <- rbind(Y1, Y2)
## Community stack plot (inset of A)
Yreg <- Y2 + Y1
Yplot <- melt(rbind(
  cbind(as.data.frame(Y2), time=seq(1,t,1), site=rep("Community 1", t)), 
  cbind(as.data.frame(Y1), time=seq(1,t,1), site=rep("Community 2", t)),
  cbind(as.data.frame(Yreg), time=seq(1,t,1), site=rep("Metacommunity", t))), id.vars = c("time", "site"))
p0 <- ggplot(Yplot, aes(x=time, y=value, fill=variable)) + 
  geom_area(stat="identity") + facet_wrap( ~ site) +
  scale_fill_manual(values=c("steelblue2", "coral2", "palegreen2", "khaki1"), name="") +
  xlab("Time") + ylab("Biomass") + ggtitle("") + theme_bw() +
  theme(legend.position="none") + 
  theme(panel.background=element_rect(fill="transparent")) +
  theme(plot.background=element_rect(fill="transparent", color = NA)) +
  theme(strip.text.x=element_text(size=6)) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), 
        panel.background=element_blank())

#### Simulation of mirrored composition across 99 randomizations ####
## Simulation 1: mirrored composition & fixed total biomass
## Simulation 2: mirrored composition & random total biomass in Y1 and Y2 at each iteration
## We generate several random mirrored communities (within the range of possible values) to explore variation 
fmin <- 0.1; fmax <- 0.4  # has to be between 0.1-0.4 otherwise negative relative frequencies can arise in Y2 (see below)
rand <- 99
res1 <- array(NA, dim=c(nbit,6,rand))
res2 <- array(NA, dim=c(nbit,6,rand))
for(k in 1:rand){
  cat(paste("Random set of communites =", k), sep="\n")
  #### Generate two community Y1 & Y2 with mirrored composition
  ## We randomly generate two mirrored community (Y1 & Y2) of 4 species (S1-4)
  ## First we define random but FIXED species relative frequency in the metacommunity
  ## FIXED species relative frequencies ensure that metacommunity compositional variability is NULL
  ## FIXED Species relative frequencies ranged from Fmin to Fmax
  Fmin <- 0.25; Fmax <- 0.25
  S1 <- rep(runif(1, Fmin, Fmax), t)
  S2 <- rep(runif(1, Fmin, min(1-S1, Fmax)), t)
  S3 <- rep(runif(1, Fmin, min(1-(S1+S2), Fmax)), t)
  S4 <- 1-(S1+S2+S3)
  Yreg <- matrix(c(S1, S2, S3, S4), nrow=t, ncol=4)
  ## Second we randomly generate species relative frequency in Y1
  ## Species relative frequencies range between fmin and fmax
  toto <- mirrored.com(Yreg)
  Y1freq <- toto[[1]]
  Y2freq <- toto[[2]]
  
  #### Generate total community biomass in Y1 and Y2
  ## Random values
  Y1 <- Y1freq * rnorm(t, sd=sd_com, mean=mu_com)
  Y2 <- Y2freq * rnorm(t, sd=sd_com, mean=mu_com)
  Y <- rbind(Y1, Y2)
  
  #### Mirrored composition & fixed total biomass
  ## We now stack the same mirrored community Y2 to Y1 nbit times 
  prog.bar <- txtProgressBar(min=1, max=nbit, style=3)
  for(i in 1:nbit){
    Ytemp <- do.call(rbind, replicate(i, Y2, simplify=FALSE)) 
    Y <- rbind(Y1,Ytemp)
    res1[i,,k] <- as.numeric(space_stab(Y, (i+1), t))
    setTxtProgressBar(prog.bar, i)
  }
  
  #### Mirrored composition & random total biomass in Y1 and Y2 at each iteration
  ## We now stack the same mirrored community Y2 to Y1 nbit times 
  ## At each iteration the total biomass within Y1 and Y2 is randomly selected
  prog.bar <- txtProgressBar(min=1, max=nbit, style=3)
  for(i in 1:nbit){
    Ytemp <- do.call(rbind, replicate(i, Y2freq * rnorm(t, sd=sd_com, mean=mu_com), simplify=FALSE)) 
    Y <- rbind(Y1freq * rnorm(t, sd=sd_com, mean=mu_com), Ytemp)
    res2[i,,k] <- as.numeric(space_stab(Y, (i+1), t))
    setTxtProgressBar(prog.bar, i)
  }
}

## Summarize and format results across randomizations
res1.mean <- apply(res1, 1:2, mean)
res1.mean <- melt(cbind(as.data.frame(res1.mean[,c(3,6)]), nbit=as.numeric(1:nbit), sim="sim1"), id.vars=c("sim", "nbit"), value.name="mean")
res1.lw95 <- apply(res1, 1:2, function(x) quantile(x, 0.025))
res1.lw95 <- melt(cbind(as.data.frame(res1.lw95[,c(3,6)]), nbit=as.numeric(1:nbit), sim="sim1"), id.vars=c("sim", "nbit"), value.name="lw95")
res1.up95 <- apply(res1, 1:2, function(x) quantile(x, 0.975))
res1.up95 <- melt(cbind(as.data.frame(res1.up95[,c(3,6)]), nbit=as.numeric(1:nbit), sim="sim1"), id.vars=c("sim", "nbit"), value.name="up95")
res1f <- cbind(res1.mean, lw95=res1.lw95$lw95, up95=res1.up95$up95)
## plot
p3 <- 
  ggplot(res1f, aes(x=nbit, y=mean, group=variable)) + 
  geom_ribbon(aes(ymin=lw95, ymax=up95, fill=variable), alpha=0.3) +
  geom_point(aes(color=variable), size=1) + geom_line(aes(linetype=variable, color=variable)) + 
  scale_linetype_manual(name="", values=c("dashed", "solid"), labels=expression(phi1, BD[phi1]^h)) +  
  scale_color_manual(name="", values=c("#B47846", "steelblue"), labels=expression(phi1, BD[phi1]^h)) +
  scale_fill_manual(name="", values=c("#B47846", "steelblue"), labels=expression(phi1, BD[phi1]^h)) +  
  ylim(0,1) +
  geom_text(x=0, y=1, label="(A)", size=4) +
  xlab("Number of stacked community 2") + ylab("Spatial synchrony") +
  labs(title="Identical total biomass across iterations") + theme_bw() +
  theme(legend.position="bottom")

## add inset p0 and arrows 
p3 <- p3 + annotation_custom(ggplotGrob(p0), xmin=10, xmax=95, ymin=0, ymax=0.58) +
  geom_point(x=1, y=subset(res1.mean, nbit==1 & variable=="V2")$mean, pch=21, fill=NA, size=4, colour="red", stroke=1) +
  geom_segment(x=1, y=subset(res1.mean, nbit==1 & variable=="V2")$mean, xend=10, yend=0.075, arrow=arrow(length=unit(0.5, "cm")), colour="red")

## Summarize and format results across randomizations
res2.mean <- apply(res2, 1:2, mean)
res2.mean <- melt(cbind(as.data.frame(res2.mean[,c(3,6)]), nbit=as.numeric(1:nbit), sim="sim1"), id.vars=c("sim", "nbit"), value.name="mean")
res2.lw95 <- apply(res2, 1:2, function(x) quantile(x, 0.025, na.rm=TRUE))
res2.lw95 <- melt(cbind(as.data.frame(res2.lw95[,c(3,6)]), nbit=as.numeric(1:nbit), sim="sim1"), id.vars=c("sim", "nbit"), value.name="lw95")
res2.up95 <- apply(res2, 1:2, function(x) quantile(x, 0.975, na.rm=TRUE))
res2.up95 <- melt(cbind(as.data.frame(res2.up95[,c(3,6)]), nbit=as.numeric(1:nbit), sim="sim1"), id.vars=c("sim", "nbit"), value.name="up95")
res2f <- cbind(res2.mean, lw95=res2.lw95$lw95, up95=res2.up95$up95)

p4 <- ggplot(res2f, aes(x=nbit, y=mean, group=variable)) + 
  geom_ribbon(aes(ymin=lw95, ymax=up95, fill=variable), alpha=0.3) +
  geom_point(aes(color=variable), size=1) + geom_line(aes(linetype=variable, color=variable)) + 
  scale_linetype_manual(name="", values=c("dashed", "solid"), labels=expression(phi1, BD[phi1]^h)) +  
  scale_color_manual(name="", values=c("#B47846", "steelblue"), labels=expression(phi1, BD[phi1]^h)) +
  scale_fill_manual(name="", values=c("#B47846", "steelblue"), labels=expression(phi1, BD[phi1]^h)) +  
  ylim(0,1) +
  geom_text(x=0, y=1, label="(B)", size=4) +
  xlab("Number of stacked community 2") + ylab("Spatial synchrony") +
  labs(title="Random total biomass at each iteration") + theme_bw() +
  theme(legend.position="bottom")

#### FIXED species relative frequencies at the metacommunity scale but increasing aggregate metacommunity variability ####
rand <- 99
## Species relative frequencies at the metacommunity scale are
## FIXED so metacommunity compositional variability SHOULD be null 
Yreg.rel <- matrix(0.25, nrow=t, ncol=4)
## sd of metacommunity biomass ranges from:
sd_com <- seq(1, 30, .25)
res3 <- array(NA, dim=c(length(sd_com),6,rand))
for(k in 1:rand){
  cat(paste("Randomization =", k), sep="\n")
  prog.bar <- txtProgressBar(min=1, max=length(sd_com), style=3)
  for(i in 1:length(sd_com)){
    repeat{
      ## Step 1: generate total biomass at the metacommunity scale ~N(60, sd_com)
      Yreg <- rnorm(t, sd=sd_com[i], mean=60)
      ## generate total biomass in Y1 ~N(30, sqrt(sd_com))
      tot1 <- rnorm(t, sd=sqrt(sd_com[i]), mean=30)
      ## retrieve total biomass in Y2
      tot2 <- Yreg-tot1
      ## Step 2: generate species relative frequency in Y1
      ## Random species relative frequencies range between fmin and fmax
      fmin <- 0.1; fmax <- 0.4
      S1 <- runif(t, fmin, fmax)
      S2 <- runif(t, fmin, min(1-S1, fmax))
      S3 <- runif(t, fmin, min(1-(S1+S2), fmax))
      S4 <- 1-(S1+S2+S3)
      Y1.rel <- matrix(c(S1, S2, S3, S4), nrow=t, ncol=4)
      Y1 <- tot1 * Y1.rel
      ## Step 3: compute species relative frequencies in Y2 so it is mirrored of Y1
      Y2 <- (Yreg.rel*(tot1+tot2)-Y1)
      if(sum(Y2 < 0) == 0) break
    }
    res3[i,,k] <- as.numeric(space_stab(rbind(Y1,Y2), 2, t))
    setTxtProgressBar(prog.bar, i)
  }
}

## Summarize and format results across randomizations
res3.mean <- apply(res3, 1:2, mean)
res3.mean <- melt(cbind(as.data.frame(res3.mean[,c(3,6)]), sd_com=sd_com), id.vars=c("sd_com"), value.name="mean")
res3.lw95 <- apply(res3, 1:2, function(x) quantile(x, 0.025, na.rm=TRUE))
res3.lw95 <- melt(cbind(as.data.frame(res3.lw95[,c(3,6)]), sd_com=sd_com), id.vars=c("sd_com"), value.name="lw95")
res3.up95 <- apply(res3, 1:2, function(x) quantile(x, 0.975, na.rm=TRUE))
res3.up95 <- melt(cbind(as.data.frame(res3.up95[,c(3,6)]), sd_com=sd_com), id.vars=c("sd_com"), value.name="up95")
res3f <- cbind(res3.mean, lw95=res3.lw95$lw95, up95=res3.up95$up95)

## plot
p5 <- 
  ggplot(res3f, aes(x=sd_com, y=mean, group=variable)) + 
  geom_ribbon(aes(ymin=lw95, ymax=up95, fill=variable), alpha=0.3) +
  geom_point(aes(color=variable), size=1) + geom_line(aes(linetype=variable, color=variable)) + 
  scale_linetype_manual(name="", values=c("dashed", "solid"), labels=expression(phi1, BD[phi1]^h)) +  
  scale_color_manual(name="", values=c("#B47846", "steelblue"), labels=expression(phi1, BD[phi1]^h)) +
  scale_fill_manual(name="", values=c("#B47846", "steelblue"), labels=expression(phi1, BD[phi1]^h)) +  
  ylim(0,1) +
  geom_text(x=1, y=1, label="(C)", size=4) +
  xlab(expression(sigma["Metacommunity biomass"])) + ylab("Spatial synchrony") +
  labs(title="Increasing variability in metacommunity biomass") + theme_bw() +
  theme(legend.position="bottom")

#### Figure 2 ####
setwd(here("Figures"))
pdf("Figure2.pdf", width=15, height=5)
ggarrange(p3, p4, p5, ncol=3, common.legend=TRUE, legend="bottom")
dev.off()

## END