compo_stab <- function(Y, s, t)
# WARNING: FUNCTION REDUNDANT WITH ERIC's ONE
#
# function to compute alpha, beta and gamma compositional variability 
#
# Arguments --
#
# Y: community table: assumes row blocks corresponding to times
# s: number of site
# t: number of years
#
# Outputs --
# alpha variability =  
# gamma variability = 
# beta variability = 
# phiAgg = 
#
# Dependence / required package --
# beta.div.comp {Legendre}
#
# Reference --
#
# Legendre, etc.
# 
# Author: Thomas LAMY
# edited March 22th 2017
{

	# check that data is balanced
	if(dim(Y)[1] != s * t) cat("STOP: sites are not surveyed every years")

	## Preparing the data
	# Site level 
	SiteL <- list(); for(i in 1:s) SiteL [[i]] <- Y[c(((i-1)*t+1):(i*t)),] 
	# Metacommunity level: sum across all sites (t time steps) 
	MetacomTime <- aggregate(Y, list(time = env$time), sum)
	# Metacommunity level: average across all sites (t time steps) 
	# MetacomTime <- aggregate(Y, list(time = env$time), mean)

	## computing beta total (quantitative Sorensen = Bray-Curtis) 
	## and its two components (rich and repl) 
	# Site level
	SiteL_comp <- lapply(SiteL, function(x) { beta.div.comp(x, coef="S", quant=TRUE, save.abc=TRUE) })
	# Metacommunity level
	MetacomTime_comp <- beta.div.comp(MetacomTime[,2:(nbsp+1)], coef="S", quant=TRUE, save.abc=TRUE)

	## Extract beta total and its components
	# Site level
	SiteL_comp_part <- lapply(SiteL_comp, function(x) { as.matrix(x$part) })
	SiteL_comp_beta <- lapply(SiteL_comp, function(x) { as.matrix(x$D) })
	SiteL_comp_repl <- lapply(SiteL_comp, function(x) { as.matrix(x$repl) })
	SiteL_comp_rich <- lapply(SiteL_comp, function(x) { as.matrix(x$rich) })

	# Metacommunity level
	MetacomTime_comp_part <- MetacomTime_comp$part
	MetacomTime_comp_beta <- as.matrix(MetacomTime_comp$D)
	MetacomTime_comp_repl <- as.matrix(MetacomTime_comp$repl)
	MetacomTime_comp_rich <- as.matrix(MetacomTime_comp$rich)

	## Get alpha, beta and phi components of compositional variability
	# AlphaComp = average beta across sites
	AlphaComp_beta <- apply(do.call(cbind, SiteL_comp_part), 1, mean)[1]
	AlphaComp_repl <- apply(do.call(cbind, SiteL_comp_part), 1, mean)[2]
	AlphaComp_rich <- apply(do.call(cbind, SiteL_comp_part), 1, mean)[3]
	# GammaComp = metacommunity level beta
	GammaComp_beta <- MetacomTime_comp_part[1]
	GammaComp_repl <- MetacomTime_comp_part[2]
	GammaComp_rich <- MetacomTime_comp_part[3]
	# phiComp = AlphaComp/GammaComp = 1/BetaComp, with beta = gamma / alpha (???)
	PhiComp_beta <- GammaComp_beta/AlphaComp_beta
	PhiComp_repl <- GammaComp_repl/AlphaComp_repl
	PhiComp_rich <- GammaComp_rich/AlphaComp_rich

	## summary table (only with beta total not its components repl and rich)
	# res <- data.frame(cat=c("GammaComp", "AlphaComp", "PhiComp"), val=c(GammaComp_beta, AlphaComp_beta, PhiComp_beta))
	res <- data.frame(GammaComp = GammaComp_beta, AlphaComp = AlphaComp_beta, PhiComp = PhiComp_beta)
	
	res
}
