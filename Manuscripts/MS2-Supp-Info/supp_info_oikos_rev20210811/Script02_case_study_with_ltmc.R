#################################################################################### 
## Case study to illustrate the dual nature of metacommunity variability          ##
## SBC LTER: understory macroalgal communities                                    ##
#################################################################################### 
## Thomas Lamy, April 18th 2017

#### setting R ####
# load packages
library(here)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(vegan)
library(gridExtra)
library(RColorBrewer)

## load function space_stab to compute compositional and aggregate stability across spatial scales
source(here("space_stab.R"))

## ggplot theme
gg_theme <- theme(panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(), 
                  panel.background=element_blank(), 
                  axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(face="bold", hjust=0.5, size=14))

#### load data ####
## load Package ID: knb-lter-sbc.50.7 as R object sbc.data
source(here("SBC_LTER_data.R"))


#### Format data ####
## create a unique ID for each transect
sbc.data$SU <- as.factor(paste(sbc.data$SITE, sbc.data$TRANSECT, sep="_"))
## transect to remove (e.g. sandy transects)
transects_rm <- c("IVEE_3", "IVEE_4", "IVEE_5", "IVEE_6", "IVEE_7", "IVEE_8", "SCDI_1", "SCTW_1",
                  "AHND_1", "ABUR_2", "NAPL_4")
## Changing NA by 0 (assuming that the species was not present)
sbc.data[is.na(sbc.data$SFDM),]$SFDM <- 0     # 2.8% of NA
## Filter and merge at the site level
sbc.data.algae <- sbc.data %>%
  filter(YEAR %in% c(2004:2017)) %>% 
  filter(COARSE_GROUPING == "UNDERSTORY ALGAE") %>% 
  filter(!(SP_CODE %in% c("BLD", "DIAT", "UBB", "UEC", "ZOMA"))) %>% # non-algae species
  filter(!(SU %in% transects_rm)) %>% 
  ## biomass data at the site level, data has been zero-filled
  group_by(YEAR, SITE, SP_CODE, SCIENTIFIC_NAME, COARSE_GROUPING) %>%
  summarize(VALUE=mean(SFDM)) %>% 
  rename(DATE=YEAR, SITE_ID=SITE, VARIABLE_NAME=SP_CODE) %>%
  ungroup() %>% as.data.frame() %>% droplevels

## number of transects per site
nb.su <- sbc.data %>%
  filter(YEAR %in% c(2004:2017)) %>% 
  filter(COARSE_GROUPING == "UNDERSTORY ALGAE") %>% 
  filter(!(SP_CODE %in% c("BLD", "DIAT", "UBB", "UEC", "ZOMA"))) %>% # non-algae species
  filter(!(SU %in% transects_rm)) %>% 
  ungroup() %>% as.data.frame() %>% droplevels
length(levels(nb.su$SU)) # 36
with(nb.su, table(SU, SITE))

#### wide community matrix ####
sbc.wide.algae <- dcast(sbc.data.algae, SITE_ID + DATE ~ VARIABLE_NAME, value.var="VALUE")
# create blocks corresponding to local communities
sbc.wide.algae <- sbc.wide.algae[order(sbc.wide.algae$DATE),]
sbc.wide.algae <- sbc.wide.algae[order(sbc.wide.algae$SITE_ID),]
# number of local communities (s)
s <- length(unique(sbc.wide.algae$SITE_ID))
# number of sampled years (t)
t <- length(unique(sbc.wide.algae$DATE))
# community matrix (Y)
Y <- sbc.wide.algae[,3:dim(sbc.wide.algae)[2]]
# removing species never sampled
which(apply(Y, 2, sum) == 0)
# PHTO SELO 
# 40   51 
Y <- Y[,which(apply(Y,2, sum) != 0)]   # 55 species
# choose color for each local community
col <- brewer.pal(s, "Set3")

#### figures: local and regional variability ####
# local scale
pA <- ggplot(sbc.data.algae, aes(x=DATE, y=VALUE, fill=VARIABLE_NAME)) + 
  geom_area(stat="identity") +
  facet_wrap(~SITE_ID) +
  scale_fill_discrete(guide=FALSE) +
  xlab("Year") + ylab(expression(paste("Community biomass (g dry", .m^-2, ")"))) + 
  labs(title="(A) Local communities") +
  theme_bw() + gg_theme

# regional scale
plot.comp.regional <- sbc.data.algae %>%
  group_by(DATE, VARIABLE_NAME) %>%
  summarize(bio=sum(VALUE)) %>%
  ungroup() %>% as.data.frame() 
pB <- ggplot(plot.comp.regional, aes(x=DATE, y=bio, fill=VARIABLE_NAME)) + 
  geom_area(stat="identity") +
  scale_fill_discrete(guide=FALSE) +
  xlab("Year") + ylab(expression(paste("Metacommunity biomass (g dry", .m^-2, ")"))) + 
  labs(title="(B) Metacommunity") +
  theme_bw() + gg_theme

#### Compute metacommunity variability ####
res <- space_stab(Y, s, t)
mult <- melt(res)
pC <- 
ggplot(mult, aes(x=factor(variable), y=value, fill=factor(variable))) + geom_bar(stat="identity", color="black") +
  geom_text(aes(label=round(value,3)), vjust=-0.3, size=2.5) +
  ylim(0, max(mult$value)+0.1) +
  scale_x_discrete(labels=c(expression(CV[gamma]^{2}), expression(CV[alpha]^{2}), expression(phi1),
    expression(BD[gamma]^{h}), expression(BD[alpha]^{h}), expression(BD[phi1]^{h}))) +
  scale_fill_manual(values=c(rep("#B47846",3), rep("steelblue", 3))) + 
  xlab("") + ylab("") + 
  labs(title="(C) Partitioning") +
  theme_bw() + gg_theme + theme(legend.position="none") +
  theme(axis.text.x=element_text(size=12))


#### Multivariate dataset ####
# List of local communities
SiteL <- list(); for(i in 1:s) SiteL [[i]] <- Y[c(((i-1)*t+1):(i*t)),] 
# Metacommunity (sum across all local communities)
MetacomTime <- Reduce("+", SiteL) 
# hellinger transformation metacommunity
bio_h_meta <- decostand(MetacomTime, "hellinger")
# combined dataset of local communities and metacommunity
Y.all <- rbind(Y, MetacomTime)
bio.h.all <- decostand(Y.all, method="hell")

#### NMDS ####
nmds2k.all <- metaMDS(bio.h.all, distance="euclidean", trace=TRUE, trymax=1000, k=2)
stressplot(nmds2k.all, dist(bio.h.all)) # stress = 0.180
# extract scrs
sites_scrs <- as.data.frame(scores(nmds2k.all, display="sites"))
spps_scrs  <- as.data.frame(scores(nmds2k.all, display="species"))
spps_scrs$Species <- rownames(spps_scrs)
# compute axis ranges
xlim_scrs <- range(sites_scrs[,1], sites_scrs[,1])
ylim_scrs <- range(sites_scrs[,2], sites_scrs[,2])
# segments between local points
segments.loc <- data.frame()
for(j in 1:s) for(i in (t*(j-1)+1):(t*j-1)) segments.loc <- rbind(segments.loc, data.frame(x=sites_scrs[i,1], y=sites_scrs[i,2], xend=sites_scrs[i+1,1], yend=sites_scrs[i+1,2], col=col[j])) 
# start points
points.start <- data.frame()
for(j in 1:s) points.start <- rbind(points.start, data.frame(x=sites_scrs[t*(j-1)+1,1], y=sites_scrs[t*(j-1)+1,2], col=col[j]))
# add start point for the metacommunity
points.start <- rbind(points.start, data.frame(x=sites_scrs[t*11+1,1], y=sites_scrs[t*11+1,2], col="black"))
sbc.wide.algae$SITE_ID <- as.factor(sbc.wide.algae$SITE_ID)
points.start$Site <- as.factor(c(levels(sbc.wide.algae$SITE_ID), "Metacommunity"))
points.start$Site <- ordered(points.start$Site, levels=c(levels(sbc.wide.algae$SITE_ID), "Metacommunity"))
# end arrows
points.end <- data.frame()
for(j in 1:s) points.end <- rbind(points.end, data.frame(x=sites_scrs[t*j-1,1], y=sites_scrs[t*j-1,2], xend=sites_scrs[t*j,1], yend=sites_scrs[t*j,2], col=col[j])) 
# segments for the metacommunity
segments.reg <- data.frame()
for(i in (t*11+1):(t*12-1)) segments.reg <- rbind(segments.reg, data.frame(x=sites_scrs[i,1], y=sites_scrs[i,2], xend=sites_scrs[i+1,1], yend=sites_scrs[i+1,2], col="black"))
# actual plot
pD <- 
  ggplot() +
  geom_point(data=points.start, aes(x=x, y=y, col=Site), size=5) +
  # add or remove species scores
  # geom_text(data=spps_scrs, aes(x=NMDS1, y=NMDS2, label=Species), size=3, col="grey") +
  geom_segment(data=segments.loc, aes(x=x, y=y, xend=xend, yend=yend), col=segments.loc$col, size=0.75, alpha=0.75) +
  geom_segment(data=points.end, aes(x=x, y=y, xend=xend, yend=yend), col=points.end$col, arrow=arrow(length=unit(0.55, "cm"), type="closed"), size=0.75) +
  geom_segment(data=segments.reg, aes(x=x, y=y, xend=xend, yend=yend), col="black", size=2) +
  geom_segment(data=points.end, aes(x=sites_scrs[t*12-1,1], y=sites_scrs[t*12-1,2], xend=sites_scrs[t*12,1], yend=sites_scrs[t*12,2]), col="black", 
               arrow=arrow(length=unit(0.5, "cm"), type="closed")) +
  xlab("Axis 1") + ylab("Axis 2") + 
  labs(title="(D) NMDS (stress = 0.18)") +
  coord_equal() +
  theme_bw() + gg_theme + 
  theme(legend.position="right", legend.title=element_blank()) +
  scale_color_manual(values=c(c(col, "black")))

#### Figure 3 ####
setwd(here("Figures"))
pdf("Figure3.pdf", width=12, height=6)
grid.arrange(pA, pB, pC, pD, 
             ncol=10, nrow=5, 
             layout_matrix=rbind(c(1,1,1,1,1,4,4,4,4,4),
                                 c(1,1,1,1,1,4,4,4,4,4),
                                 c(1,1,1,1,1,4,4,4,4,4),
                                 c(2,2,2,3,3,4,4,4,4,4),
                                 c(2,2,2,3,3,4,4,4,4,4)))
dev.off()  
                           
## End
