lcbdLTER <- function(Y, s=s, t=t, method="%difference", plot=TRUE)
# Compute LCBD 
# See Legendre, P. and M. De Cáceres. 2013. Beta diversity as the variance of community data: dissimilarity coefficients and partitioning. Ecology Letters 16: 951-963.
# Plot From Lamy et al. 2013. Understanding the spatio-temporal response of coral Reef fish communities to natural disturbances: insights from beta-diversity decomposition. Plos One 10: e0138696
# add some bla bla
# 
{

## load needed package
library (gplots)   # plotCI()
## load dependancies
	## -- the following functions are required and need to be loaded
source("./utilities/beta_diversity_function/beta.div.R")
source("./utilities/beta_diversity_function/beta.div.comp.R")
source("./utilities/beta_diversity_function/LCBD.comp.R")

	## Computing LCBD for all space-time combinations
	res <- beta.div(Y, method=method, sqrt.D=FALSE, samp=TRUE, nperm=999, save.D=TRUE, clock=FALSE)
	## mean LCBD per time step
	mean_time = tapply(res$LCBD, as.numeric(T), mean)
	sd_time = tapply(res$LCBD, as.numeric(T), sd)
	## mean LCBD per spatial unit
	mean_site = tapply(res$LCBD, A, mean)
	sd_site = tapply(res$LCBD, A, sd)

	#rct = raup_crick(Y.t, plot_names_in_col1=FALSE, classic_metric=FALSE, split_ties=TRUE, reps=9999, report_similarity=FALSE)
	LCBD.spacetime = data.frame(LCBD=res$LCBD, p.LCBD=res$p.LCBD)
	LCBD.time = data.frame(LCBD.mean=mean_time, LCBD.sd=sd_time)
	LCBD.space = data.frame(LCBD.mean=mean_site, LCBD.sd=sd_site)

	if(plot==TRUE){
		par(fig=c(0,0.8,0,0.8),mar=c(3,3,0.5,0.5), oma=c(1,1,1,1), mgp=c(1.75,0.5,0), cex.lab=1)
		plot(as.numeric(T), as.numeric(S), type="n", xaxt="n",yaxt="n", xlab="Year", ylab="Site", main="")
		abline(h=c(1:s), lty=2, col="grey")
		abline(v=c(1:t), lty=2, col="grey")
		axis(1,labels=seq(1, t, by=1),at=seq(1, t, by=1),tck=-0.01, cex.axis=0.8)
		axis(2,labels=seq(1, s, by=1),at=seq(1, s, by=1), las=1,tck=-0.01, cex.axis=0.8)
		points(as.numeric(T), as.numeric(S), type="p", pch=16, cex=32*sqrt(res$LCBD), col="steelblue2")
		points(as.numeric(T[which(res$p.LCBD<0.05)]), as.numeric(S[which(res$p.LCBD<0.05)]),
		type="p", pch=21, cex=32*sqrt(res$LCBD[which(res$p.LCBD<0.05)]), col="black")

		par(fig=c(0,0.8,0.7,1), cex.lab=0.7, new=TRUE)
		plotCI(seq(1,t,by=1), mean_time, uiw = sd_time, type="p", pch=16, lty = 3, bty = "n", xaxt ="n",yaxt="n", gap = 0,
		ylab="Year LCBD", xlab="", main = "", col="black", lwd=1)
		axis(2,labels=c(4,6,8),at=c(0.004,0.006,0.008), las=1,tck=-0.01, cex.axis=0.8)

		par(fig=c(0.7,1,0,0.8), cex.lab=0.7, new=TRUE)
		plotCI(mean_site, c(1:11), uiw = sd_site, err="x", type="p", pch=16, lty = 3, bty = "n", xaxt ="n",yaxt="n", gap = 0,
		ylab="", xlab="Site LCBD", main = "", col="black", lwd=1)
		axis(1,labels=c(4,6,8),at=c(0.004,0.006,0.008), las=1,tck=-0.01, cex.axis=0.8)
	}

	out <- list(spacetime=LCBD.spacetime, time=LCBD.time, space=LCBD.space)
	out
}


