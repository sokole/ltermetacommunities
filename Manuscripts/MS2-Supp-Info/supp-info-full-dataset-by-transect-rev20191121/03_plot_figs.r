#' ################################################
#' Data plotting script
#' 
#' Last updated -- Eric Sokol -- 12 June 2019
#' 
#' based on script by
#'   Thomas Lamy
#' ###############################################

# Clean out workspace
rm(list = ls())
gc()

# required libraries
library(grid)
library(vegan)
library(tidyverse)
library(reshape2)

#########################################
# read in data
#################

data.long <- read_csv('DATA_long.csv')
data.wide <- read_csv('DATA_wide.csv')

plot.tot.bio.local <- read_csv('DATA_plot.tot.bio.local.csv')
plot.tot.bio.regional <- read_csv('DATA_plot.tot.bio.regional.csv')

metacomm.var <- read_csv('RESULTS_metacomm_var__ltmc.csv')

#########################################
# ggplot theme
##################
gg_theme <- theme(panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(), 
                  panel.background=element_blank(), 
                  axis.text=element_text(size=14),
                  axis.title=element_text(size=16, face="bold"),
                  axis.line=element_line(colour="black"),
                  panel.border=element_rect(colour="black", fill=NA, size=1))

# ############################################
# ## plot total biomass
# #########################
# plot_total_biomass <- ggplot(plot.tot.bio.local, aes(x=DATE, y=tot.bio)) +
#   geom_line(aes(colour = SITE_ID), size = 1) +
#   geom_line(data=plot.tot.bio.regional, aes(x=DATE, y=tot.bio), colour = "black", size = 3) +
#   xlab("Year") + ylab(expression(bold(paste("Total community biomass (g dry", .m^-2, ")")))) + labs(title = "") +
#   gg_theme + theme(axis.text=element_text(size=14), axis.title=element_text(size=18,face="bold"))
# plot_total_biomass
# # dev.off()

##########################################################
#### figures: compositional variability ####
#############################################

# local scale
# setwd(paste(my_wd, "Figures", sep=""))
# pdf("Compositional_metric_local.pdf", width=8, height=5)
plot_local_communities <- ggplot(data.long, aes(x = DATE, y = VALUE, fill = SP_CODE)) +
  geom_area(stat = "identity") +
  facet_wrap( ~ SITE_ID) +
  scale_fill_discrete(guide=FALSE) +
  xlab("Year") + ylab(expression(bold(paste("Total community biomass (g dry ", m^-2, ")")))) + labs(title = "") +
  gg_theme + theme(axis.text = element_text(size=8), axis.title = element_text(size=16,face="bold"))

# # view in windows
# windows(7, 5)
# print(plot_local_communities)
# dev.off()

# print pdf
pdf(file = 'FIG_local_community_composition_by_time.pdf',
    width = 7, height = 5)
print(plot_local_communities)
dev.off()

# regional scale
plot.comp.regional <- data.long %>%
  group_by(DATE, SP_CODE) %>%
  summarize(bio = sum(VALUE)) %>%
  ungroup() %>% as.data.frame()
# setwd(paste(my_wd, "Figures", sep=""))
# pdf("Compositional_metric_regional.pdf", width=6, height=5)
plot_metacommunity <- ggplot(plot.comp.regional, aes(x = DATE, y = bio, fill = SP_CODE)) +
  geom_area(stat = "identity") +
  scale_fill_discrete(guide=FALSE) +
  xlab("Year") + ylab(expression(bold(paste("Total metacommunity biomass (g dry ", m^-2, ")")))) + labs(title = "") +
  gg_theme + theme(axis.text = element_text(size=8), axis.title = element_text(size=16,face="bold"))

# # view in windows
# windows(6, 5)
# print(plot_metacommunity)
# dev.off()

# print pdf
pdf(file = 'FIG_metacommunity_composition_by_time.pdf',
    width = 7, height = 5)
print(plot_metacommunity)
dev.off()

# # Alternative plot
# time <- rep(seq(1,t,1),s)
# site <- c(); for(i in 1:s) site <- c(site, rep(paste("Site", i), t))
# Yplot <- melt(rbind(
#   cbind(as.data.frame(Y), time=time, site=site),
#   cbind(as.data.frame(MetacomTime), time=seq(1,t,1), site=rep("Region", t))), id.vars=c("time", "site"))
# pp1 <- ggplot(Yplot, aes(x=time, y=value, fill=variable)) +
#   geom_area(stat="identity") +
#   facet_wrap(~site) +
#   scale_fill_discrete(guide=FALSE) +
#   xlab("Time") + ylab("Biomass/Abundance") + labs(title = "") +
#   theme(panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         panel.background=element_blank(),
#         axis.text=element_text(size=8),
#         axis.title=element_text(size=16,face="bold"),
#         axis.line=element_line(colour="black"),
#         panel.border=element_rect(colour="black", fill=NA, size=1))


#############################################
# plot metacomm var results
########################

metacomm_var_labels <- c(expression(CV[alpha]^{2}), expression(CV[gamma]^{2}), expression(CV[phi]^{2}),
                         expression(BD[alpha]^{hT}), expression(BD[gamma]^{hT}), expression(BD[phi]^{hT}),
                         expression(BD[alpha]^{h}), expression(BD[gamma]^{h}), expression(BD[phi]^{h}))

metacomm.var$variable_factor_ordered <- factor(
  metacomm.var$variable,
  ordered = TRUE,
  levels = c('AlphaCV','GammaCV','PhiCV',
             'AlphaTBD','GammaTBD','PhiTBD',
             'AlphaHBD','GammaHBD','PhiHBD'))

metacomm.var$var_type <- c('CV', 'CV','CV',
                           'TBD','TBD','TBD',
                           'HBD','HBD','HBD')

pp2 <- ggplot(metacomm.var, aes(x=variable_factor_ordered, y=value, fill=factor(var_type))) + 
  geom_bar(stat="identity", color="black") +
  geom_text(aes(label=round(value,3)), vjust=-0.3, size=3.5) +
  ylim(0, max(metacomm.var$value)+0.1) +
  scale_x_discrete(labels=metacomm_var_labels) +
  scale_fill_manual(values=c("coral2","steelblue2","lightsteelblue")) + 
  xlab("") + ylab("Metric value") + labs(title = "") +
  gg_theme + theme(axis.text=element_text(size=10), 
                   axis.title=element_text(size=16,face="bold"),
                   legend.position="none")

# view in windows
# windows(6, 5)
# print(pp2)
# dev.off()

# print pdf
pdf(file = 'FIG_metacommunity_variability_metrics.pdf',
    width = 6, height = 5)
print(pp2)
dev.off()

#######################################################
# NMDS
######

# # pdf is too big
# pdf(file = 'FIG_NMDS.pdf',
#     width = 5, height = 5)



# windows(width = 7, height = 7)

# number of local communities (s)
s <- length(unique(data.wide$SITE_ID))
# number of sampled years (t)
t <- length(unique(data.wide$DATE))
# community matrix (Y)
Y <- data.wide[,3:dim(data.wide)[2]]
# removing species never sampled
which(apply(Y, 2, sum) == 0)
# PHTO SELO 
# 89  109 
Y <- Y[,which(apply(Y,2, sum) != 0)]   # 124 species / 122 species

# Local communities
SiteL <- list(); for(i in 1:s) SiteL[[i]] <- Y[c(((i-1)*t+1):(i*t)),] 
col <- c(rainbow(s), "black")
# Metacommunity
MetacomTime <- Reduce("+", SiteL) 
Y.all <- rbind(Y, MetacomTime)
# Hellinger distance
bio.h.all <- decostand(Y.all, method="hell")

# Nonmetric Multidimensional Scaling (NMDS)
set.seed(1234)
nmds2k.all <- metaMDS(bio.h.all, distance="euclidean", trace=TRUE, trymax=1000, k=2)

jpeg(file = 'FIG_NMDS_stress_plot.jpg',
     width = 7, height = 7,
     res = 300,
     units = 'in')
stressplot(nmds2k.all, dist(bio.h.all))
dev.off()

# extract scrs
sites_scrs <- scores(nmds2k.all, display="sites")
spps_scrs  <- scores(nmds2k.all, display="species")
# compute axis ranges
xlim_scrs <- range(sites_scrs[,1], sites_scrs[,1])
ylim_scrs <- range(sites_scrs[,2], sites_scrs[,2])

# plot
# layout(matrix(c(1, 2, 1, 3), ncol=2, byrow=TRUE), widths=c(2, 1))
jpeg(file = 'FIG_NMDS.jpg',
     width = 7, height = 7,
     res = 300,
     units = 'in')

plot(nmds2k.all, type="n", 
     xlim=c(xlim_scrs[1] - .1, xlim_scrs[2]+.75), 
     ylim=ylim_scrs, yaxt="n", xaxt="n", ylab="Axis 2", xlab="Axis 1", main="")
axis(1,labels=seq(-2, 2, by=0.2),at=seq(-2, 2, by=0.2),tck=-0.01, cex.axis=1)
axis(2,labels=seq(-2, 2, by=0.2),at=seq(-2, 2, by=0.2), las=1, tck=-0.01, cex.axis=1)

# plot observations
# for(i in 1:(s+1)) text(sites_scrs[(t*(i-1)+1):(t*i),1],sites_scrs[(t*(i-1)+1):(t*i),2], paste("t", seq(1,t,1), sep=""), col=col[i], cex=0.8)

for(j in 1:s) for(i in (t*(j-1)+1):(t*j-1)) segments(sites_scrs[i,1], sites_scrs[i,2], sites_scrs[i+1,1],sites_scrs[i+1,2], lwd=2, col=col[j])
# start point
for(j in 1:s) points(sites_scrs[t*(j-1)+1,1], sites_scrs[t*(j-1)+1,2], col=col[j], pch=16, cex=1.5)
# end point
for(j in 1:s) arrows(sites_scrs[t*j-1,1], sites_scrs[t*j-1,2], sites_scrs[t*j,1],sites_scrs[t*j,2], length=0.1, angle=30, code=2, col=col[j], lwd=2)
# metacommunity
for(i in (t*s+1):(t*(s+1)-1)) segments(sites_scrs[i,1], sites_scrs[i,2], sites_scrs[i+1,1],sites_scrs[i+1,2], lwd=4, col="black")
points(sites_scrs[(t*s+1),1], sites_scrs[(t*s+1),2], col="black", pch=16, cex=1.5)
arrows(sites_scrs[(t*(s+1)-1),1], sites_scrs[(t*(s+1)-1),2], sites_scrs[(t*(s+1)),1], sites_scrs[(t*(s+1)),2], length=0.1, angle=30, code=2, col="black", lwd=4)
# add species
tresh_spp <- abs(spps_scrs[,1])>0.3 & abs(spps_scrs[,2])>0.15
text(spps_scrs[tresh_spp,], row.names(spps_scrs[tresh_spp,]),  col="grey", pch="+", cex=0.8)

legend(title = 'Site ID',
       x = 'bottomright',
       col = col,
       lty = 1,
       lwd = c(rep(2, s), 4),
       legend = c(unique(data.wide$SITE_ID), 'Metacommunity'),
       bty = 'o',
       cex = .65)
# # add pp1 and pp2
# vp.TopRight <- viewport(height=unit(.5, "npc"), width=unit(0.33, "npc"), just=c("right", "top"), x=1, y=1)
# vp.BottomRight <- viewport(height=unit(.5, "npc"), width=unit(0.33, "npc"), just=c("right", "top"), x=1, y=0.5)
# print(pp2, vp=vp.TopRight)
# print(pp1, vp=vp.BottomRight)
dev.off()


