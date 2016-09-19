'PCNM' <- 
function(matdist, thresh=NULL, all=FALSE, include.zero=FALSE)
#
# Compute the PCNM or dbMEM eigenfunctions corresponding to 
# all eigenvalues (+, 0, -). 
#    In PCNM computation, the diagonal of D = 0.
#    In dbMEM, the diagonal of D = 4*threshh.
#    Distance-based MEM are described in Dray et al. 2006. 
#    The name was abbreviated to db-MEM by PPN & PL (subm.)
# Input file: distance matrix produced by the function "dist".
# Computation of the threshold requires a function of the library "ape".
# 
# Original PCNM function: Stephane Dray, November 11, 2004
# The present version: Pierre Legendre, August 2007, January and March 2009
# Modified for STI: Miquel De Caceres, April 2009
{
	require(vegan)
	epsilon <- sqrt(.Machine$double.eps) 
	a <- system.time({
	single <- FALSE

	matdist <- as.matrix(matdist)
	n <- nrow(matdist)

	# Truncation of distance matrix
	if(is.null(thresh)) {
		spanning <- vegan::spantree(as.dist(matdist))
		threshh <- max(spanning$dist)
	    cat("Truncation level =",threshh+0.000001,'\n')
		} else {
		threshh = thresh
	    cat("User-provided truncation threshold =",thresh,'\n')
		}
	matdist[matdist > threshh] <- 4*threshh

	diagonal <- 0

	mypcnm.all <- pcoa.all(matdist, diagonal=diagonal, all=all, include.zero=include.zero, rn=rownames(matdist))

	})
	a[3] <- sprintf("%2f",a[3])
	cat("Time to compute PCNMs =",a[3]," sec",'\n')
	if(is.null(thresh)) {
		res <- list(values=mypcnm.all$values, vectors=mypcnm.all$vectors, spanning=spanning, thresh=threshh+0.000001)
	} else {
		res <- list(values=mypcnm.all$values, vectors=mypcnm.all$vectors, thresh=threshh+0.000001)
	}
	res
}
