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

  ## format a time vector
  time = rep(paste("time", rep(1:t), sep=""), s)
  ## number of species
  nbsp = dim(Y)[2]
  
  ## Preparing the data
	# Site level 
	SiteL <- list(); for(i in 1:s) SiteL [[i]] <- Y[c(((i-1)*t+1):(i*t)),] 
	# Metacommunity level: sum across all sites (t time steps) 
	MetacomTime <- aggregate(Y, list(time), sum)
	# Metacommunity level: average across all sites (t time steps) 
	# MetacomTime <- aggregate(Y, list(time), mean)

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
	## AlphaComp and GammaComp are multiplied by 2 to produce normalized values in the range [0, 1] 
	# AlphaComp = average beta across sites
	AlphaComp_beta <- apply(do.call(cbind, SiteL_comp_part), 1, mean)[1] * 2
	AlphaComp_repl <- apply(do.call(cbind, SiteL_comp_part), 1, mean)[2] * 2
	AlphaComp_rich <- apply(do.call(cbind, SiteL_comp_part), 1, mean)[3] * 2
	# GammaComp = metacommunity level beta
	GammaComp_beta <- MetacomTime_comp_part[1] * 2 
	GammaComp_repl <- MetacomTime_comp_part[2] * 2
	GammaComp_rich <- MetacomTime_comp_part[3] * 2
	# phiComp = AlphaComp/GammaComp = 1/BetaComp, with beta = gamma / alpha (???)
	PhiComp_beta <- GammaComp_beta/AlphaComp_beta
	PhiComp_repl <- GammaComp_repl/AlphaComp_repl
	PhiComp_rich <- GammaComp_rich/AlphaComp_rich

	## Wang and Loreau use the squared coefficients of local and regional scales
	AlphaComp_beta2 <- AlphaComp_beta^2
	AlphaComp_repl2 <- AlphaComp_repl^2
	AlphaComp_rich2 <- AlphaComp_rich^2
	# GammaComp2
	GammaComp_beta2 <- GammaComp_beta^2
	GammaComp_repl2 <- GammaComp_repl^2
	GammaComp_rich2 <- GammaComp_rich^2
	# phiComp2
	PhiComp_beta2 <- GammaComp_beta2/AlphaComp_beta2
	PhiComp_repl2 <- GammaComp_repl2/AlphaComp_repl2
	PhiComp_rich2 <- GammaComp_rich2/AlphaComp_rich2
	
	## summary table (only with beta total not its components repl and rich)
	# res <- data.frame(cat=c("GammaComp", "AlphaComp", "PhiComp"), val=c(GammaComp_beta, AlphaComp_beta, PhiComp_beta))
	res <- data.frame(GammaComp = GammaComp_beta, AlphaComp = AlphaComp_beta, PhiComp = PhiComp_beta,
	                  GammaComp2 = GammaComp_beta2, AlphaComp2 = AlphaComp_beta2, PhiComp2 = PhiComp_beta2,
	                  GammaComp_repl = GammaComp_repl, AlphaComp_repl = AlphaComp_repl, PhiComp_repl = PhiComp_repl,
	                  GammaComp_repl2 = GammaComp_repl2, AlphaComp_repl2 = AlphaComp_repl2, PhiComp_repl2 = PhiComp_repl2)
	
	res
}
