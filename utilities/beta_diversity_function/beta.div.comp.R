# Legendre, P. (2014) Interpreting the replacement and richness difference
# components of beta diversity. Global Ecology and Biogeography, 23, 1324-1334.

# Appendix S3
#
# R function to compute the Podani- or Baselga-family decomposition of the 
# Jaccard or Sørensen groups into replacement and richness difference 
# (or nestedness) components, for species presence-absence or abundance data.


beta.div.comp <- function(mat, coef="J", quant=FALSE, save.abc=FALSE)
#
# Description --
# 
# Podani-family and Baselga-family decompositions of the Jaccard and Sørensen 
# dissimilarity coefficients into replacement and richness difference 
# components, for species presence-absence or abundance data, as described  
# in Legendre (2014).
#
# Usage --
#
# beta.div.comp(mat, coef="J", quant=FALSE, save.abc=FALSE)
#
# Arguments --
#
# mat : Data in matrix or data.frame form.
# coef : Family of coefficients to be computed --
#        "S" or "Sorensen": Podani family, Sørensen-based indices
#        "J" or "Jaccard" : Podani family, Jaccard-based indices
#        "BS" : Baselga family, Sørensen-based indices
#        "BJ" : Baselga family, Jaccard-based indices
#        "N" : Podani & Schmera (2011) relativized nestedness index.
#        The quantitative form in Sørensen family is the percentage difference.
#        The quantitative form in the Jaccard family is the Ruzicka index.
#
# quant=TRUE : Compute the quantitative form of replacement, nestedness and D.
#      =FALSE: Compute the presence-absence form of the coefficients.
# save.abc=TRUE : Save the matrices of parameters a, b and c used in the
#      presence-absence calculations.
#
# Details --
#
#    For species presence-absence data, the distance coefficients are 
# Jaccard=(b+c)/(a+b+c) and Sørensen=(b+c)/(2*a+b+c) with usual abc notation.
#
#    For species abundance data, the distance coefficients are 
# the Ruzicka index = (B+C)/(A+B+C) and Odum's percentage difference 
# (incorrectly called Bray-Curtis) = (B+C)/(2A+B+C), where  
# A = sum of the intersections (or minima) of species abundances at two sites,
# B = sum at site 1 minus A, C = sum at site 2 minus A.
#
#    The binary (quant=FALSE) and quantitative (quant=TRUE) forms of the S and  
# J indices return the same values when computed for presence-absence data.
#
# Value --
#
# repl : Replacement matrix, class = 'dist'.
# rich : Richness/abundance difference or nestedness matrix, class = 'dist'.
#        With options "BJ", "BS" and "N", 'rich' contains nestedness indices.
#        With option "N", the 'repl' and 'rich' values do not add up to 'D'.
# D    : Dissimilarity matrix, class = 'dist'.
# part : Beta diversity partitioning -- 
#        1. Total beta div. = sum(D.ij)/(n*(n-1)) (Legendre & De Cáceres 2013)
#        2. Total replacement diversity 
#        3. Total richness difference diversity (or nestedness)
#        4. Total replacement div./Total beta div.
#        5. Total richness difference div. (or nestedness)/Total beta div.
# Note : Name of the dissimilarity coefficient.
#
# References --
#
# Baselga, A. (2010) Partitioning the turnover and nestedness components of beta 
# diversity. Global Ecology and Biogeography, 19, 134–143.
#
# Baselga, A. (2012) The relationship between species replacement, dissimilarity 
# derived from nestedness, and nestedness. Global Ecology and Biogeography, 21, 
# 1223–1232. 
#
# Baselga, A. (2013) Separating the two components of abundance-based 
# dissimilarity: balanced changes in abundance vs. abundance gradients. Methods 
# in Ecology and Evolution, 4, 552–557.
#
# Carvalho, J.C., Cardoso, P., Borges, P.A.V., Schmera, D. & Podani, J. (2013)
# Measuring fractions of beta diversity and their relationships to nestedness: 
# a theoretical and empirical comparison of novel approaches. Oikos, 122, 
# 825–834.
#
# Legendre, P. 2014. Interpreting the replacement and richness difference   
# components of beta diversity. Global Ecology and Biogeography, 23, 1324-1334.
#
# Legendre, P. and M. De Cáceres. 2013. Beta diversity as the variance of community data: 
# dissimilarity coefficients and partitioning. Ecology Letters 16: 951-963.
#
# Podani, J., Ricotta, C. & Schmera, D. (2013) A general framework for analyzing 
# beta diversity, nestedness and related community-level phenomena based on 
# abundance data. Ecological Complexity, 15, 52-61.
#
# Podani, J. & Schmera, D. 2011. A new conceptual and methodological framework 
# for exploring and explaining pattern in presence-absence data. Oikos, 120, 
# 1625–1638.
#
# License: GPL-2 
# Author:: Pierre Legendre
{
coef <- pmatch(coef, c("S", "J", "BS", "BJ", "N"))
if(coef==5 & quant) stop("coef='N' and quant=TRUE: combination not programmed")
mat <- as.matrix(mat)
n <- nrow(mat)
if(is.null(rownames(mat))) noms <- paste("Site",1:n,sep="")
	else noms <- rownames(mat)
#
if(!quant) {      # Binary data provided, or make the data binary
	if(coef==1) form="Podani family, Sorensen" 
	if(coef==2) form="Podani family, Jaccard"
	if(coef==3) form="Baselga family, Sorensen" 
	if(coef==4) form="Baselga family, Jaccard"
	if(coef==5) form="Podani & Schmera (2011) relativized nestedness"
	mat.b <- ifelse(mat>0, 1, 0)
	a <- mat.b %*% t(mat.b)
	b <- mat.b %*% (1 - t(mat.b))
	c <- (1 - mat.b) %*% t(mat.b)
	min.bc <- pmin(b,c)
	#
	if(coef==1 || coef==2) {
		repl <- 2*min.bc   # replacement, turnover, beta-3
		rich <- abs(b-c)   # nestedness, richness diff., beta-rich
		#
		# Add the denominators
		if(coef==1) {                # Sørensen-based components
			repl <- repl/(2*a+b+c)
			rich <- rich/(2*a+b+c)
			D <- (b+c)/(2*a+b+c)
			} else if(coef==2) {     # Jaccard-based components
			repl <- repl/(a+b+c)
			rich <- rich/(a+b+c)
			D <- (b+c)/(a+b+c)
			}
	} else if(coef==3) {     # Baselga 2010 components based on Sørensen
		D <- (b+c)/(2*a+b+c)             # Sørensen dissimilarity
		repl <- min.bc/(a+min.bc)        # replacement, turnover
		rich <- D-repl                   # richness difference
		
	} else if(coef==4) {      # Baselga 2012 components based on Jaccard
		D <- (b+c)/(a+b+c)               # Jaccard dissimilarity
		repl <- 2*min.bc/(a+2*min.bc)    # replacement, turnover
		rich <- D-repl                   # richness difference
	} else if(coef==5) {      # rich = Podani N = nestdness based on Jaccard
		repl <- 2*min.bc/(a+b+c)
		D <- (b+c)/(a+b+c)
		rich <- matrix(0,n,n)
		for(i in 2:n) {
			for(j in 1:(i-1)) {
			aa = a[i,j]; bb = b[i,j]; cc = c[i,j]
			if(a[i,j] == 0)  rich[i,j] <- 0  
				else  rich[i,j] <- (aa + abs(bb-cc))/(aa+bb+cc) 
				}
			}
		}
	
	rownames(repl) <- rownames(rich) <- rownames(D) <- noms
	D <- as.dist(D)
	repl <- as.dist(repl)
	rich <- as.dist(rich)
	total.div <- sum(D)/(n*(n-1))
	repl.div <- sum(repl)/(n*(n-1))
	rich.div <- sum(rich)/(n*(n-1))
	part <- c(total.div,repl.div,rich.div,repl.div/total.div,rich.div/total.div)
	#
	if(save.abc) {
	res <- list(repl=repl, rich=rich, D=D, part=part, Note=form, 
		a=as.dist(a), b=as.dist(b), c=as.dist(c))
	} else { 
	res <- list(repl=repl, rich=rich, D=D, part=part, Note=form)
	}
#
} else {      # Quantitative data
# Calculations based on individuals.within.species
	if(coef==1) form<-"Podani family, percentage difference" 
	if(coef==2) form<-"Podani family, Ruzicka"
	if(coef==3) form<-"Baselga family, percentage difference"
	if(coef==4) form<-"Baselga family, Ruzicka"
	# Baselga (2013) notation:
	# A = W = sum of minima in among-site comparisons
	# B = site.1 sum - W = K.1 - W
	# C = site.2 sum - W = K.2 - W
	K <- vector("numeric", n)   # site (row) sums
	W <- matrix(0,n,n)
	repl <- matrix(0,n,n)
	rich <- matrix(0,n,n)
	D <- matrix(0,n,n)
	rownames(repl) <- rownames(rich) <- rownames(D) <- noms
	K <- apply(mat,1,sum)         # Row sums
	for(i in 2:n) for(j in 1:(i-1)) W[i,j] <- sum(pmin(mat[i,], mat[j,]))
	#
	# Quantitative extensions of the S and J decompositions
	for(i in 2:n) {
		for(j in 1:(i-1)) {
			repl[i,j] <- 2*(min(K[i],K[j])-W[i,j]) # 2*min(B,C)
			rich[i,j] <- abs(K[i]-K[j])            # abs(B-C)
			}
		}
	#
	# Add the denominators
	if(coef==1) {         # Sørensen-based (% difference) components
		for(i in 2:n) {
			for(j in 1:(i-1)) {	                        # Baselga 2013 notation:
				repl[i,j] <- repl[i,j]/(K[i]+K[j])          # 2min(B,C)/(2A+B+C)
				rich[i,j] <- rich[i,j]/(K[i]+K[j])          # abs(B-C)/(2A+B+C)
				# cat(K[i], K[j], W[i,j],"\n")
				D[i,j] <- (K[i]+K[j]-2*W[i,j])/(K[i]+K[j])  # (B+C)/(2A+B+C)
				}
			}
		} else if(coef==2) {    # Jaccard-based (Ruzicka) components
		for(i in 2:n) {
			for(j in 1:(i-1)) {                         # Baselga 2013 notation:
				repl[i,j] <- repl[i,j]/(K[i]+K[j]-W[i,j])   # 2min(B,C)/(A+B+C)
				rich[i,j] <- rich[i,j]/(K[i]+K[j]-W[i,j])   # abs(B-C)/(A+B+C)
				# cat(K[i], K[j], W[i,j],"\n")
			D[i,j]<-(K[i]+K[j]-2*W[i,j])/(K[i]+K[j]-W[i,j]) # (B+C)/(A+B+C)
				}
			}
		}
	#
	# Baselga (2013): quantitative extensions of the Baselga (2010) indices
	if(coef==3) {   # Baselga (2013) indices decomposing percentage difference
		for(i in 2:n) {
			for(j in 1:(i-1)) {
				repl[i,j] <- (min(K[i],K[j])-W[i,j])/min(K[i],K[j])
				rich[i,j] <- abs(K[i]-K[j])*W[i,j]/((K[i]+K[j])*min(K[i],K[j]))
				# cat(K[i], K[j], W[i,j],"\n")
				D[i,j] <- (K[i]+K[j]-2*W[i,j])/(K[i]+K[j])
				}
			}
		}	
	if(coef==4) {   # Decomposing Ruzicka in the spirit of Baselga 2013
		for(i in 2:n) {
			for(j in 1:(i-1)) {
				repl[i,j] <- 
					2*(min(K[i],K[j])-W[i,j])/(2*min(K[i],K[j])-W[i,j])
				rich[i,j] <- abs(K[i]-K[j])*W[i,j]/
					((K[i]+K[j]-W[i,j])*(2*min(K[i],K[j])-W[i,j]))
				# cat(K[i], K[j], W[i,j],"\n")
				D[i,j] <- (K[i]+K[j]-2*W[i,j])/(K[i]+K[j]-W[i,j])
				}
			}
		}	
	#
	repl <- as.dist(repl)
	rich <- as.dist(rich)
	D <- as.dist(D)
	repl.div <- sum(repl)/(n*(n-1))
	rich.div <- sum(rich)/(n*(n-1))
	total.div <- sum(D)/(n*(n-1))
	part <- c(total.div,repl.div,rich.div,repl.div/total.div,rich.div/total.div)
	#
	res <- list(repl=repl, rich=rich, D=D, part=part, Note=form)
}
res
}