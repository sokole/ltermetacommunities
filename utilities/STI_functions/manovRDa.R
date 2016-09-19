#===============================================================================
# manovRDa: A function in R-language for two-way MANOVA-like with or with interaction
# This function is modified to work with space-time models. Space and time can be 
# considered random or fixed. Considering random factors affects models with interaction 
# and nested models.
#
# Computed as described in Legendre & Anderson (1999)
# "manovRDa" was made by modifying function "rdaTest" by Legendre et al. (2005)
# Etienne Laliberte, March 2007
# Modified by Miquel De Cáceres (Departement de sciences biologiques, Université de Montréal). February 2009
#===============================================================================
#
'manovRDa' <- function(Y, s, t, S.mat=NULL, T.mat=NULL, STI.mat=NULL, Sfixed=TRUE, Tfixed=TRUE, S.test=TRUE, T.test=TRUE, STI.test=TRUE, model = "5", nperm=999)
#
# PARAMETERS:
#
# Y     		is the site-by-species data table (assumes row blocks corresponding to times, i.e. within each block 
#				all sites occur)
# s     		the number of spatial points
# t     		the number of time campaigns
# S.mat 		the matrix of spatial variables.
# T.mat 		the matrix of temporal variables.
# STI.mat 	the matrix of interaction variables.
# Sfixed		a logical value: is Factor space fixed, or not (if FALSE, it is considered a random factor)
# Tfixed		a logical value: is Factor time fixed, or not (if FALSE, it is considered a random factor)
#
# S.test		a logical value saying whether space should be tested or not
# T.test		a logical value saying whether time should be tested or not
# STI.test	a logical value saying whether space-time interaction should be tested or not
#
# model 		space-time model (used to identify nested models "6a" and "6b")
#
# nperm 		the number of permutations to be done
#
#===============================================================================
#
{
library(MASS)

n=nrow(Y)
p=ncol(Y)
a=ncol(S.mat)
b=ncol(T.mat)

if(!is.null(STI.mat)) {c=ncol(STI.mat)}
else {c = 0}

A = S.mat
B = T.mat
if(!is.null(STI.mat)) AxB = STI.mat

# Compute projector of A and Yfit.A
invA = ginv(t(A) %*% A)
projA = A %*% invA %*% t(A)
Yfit.A = projA %*% Y

# Compute projector of B and Yfit.B
invB = ginv(t(B) %*% B)
projB = B %*% invB %*% t(B)
Yfit.B = projB %*% Y

# Compute projector of AxB and Yfit.AxB
if(!is.null(STI.mat)) {
	invAxB = ginv(t(AxB) %*% AxB)
	projAxB = AxB %*% invAxB %*% t(AxB)
	Yfit.AxB = projAxB %*% Y
}

# Create a "compound matrix" to obtain R-square and adjusted R-square
if(!is.null(STI.mat)) {
	ABAxB=cbind(A,B,AxB)
} else {
	ABAxB=cbind(A,B)
}

# Compute projector of ABAxB and Yfit.ABAxB
invABAxB = ginv(t(ABAxB) %*% ABAxB)
projABAxB = ABAxB %*% invABAxB %*% t(ABAxB)
Yfit.ABAxB = projABAxB %*% Y

# Compute Sums of squares (SS) and Mean squares (MS)
SS.Y = sum(Y*Y)
SS.Yfit.ABAxB = sum(Yfit.ABAxB*Yfit.ABAxB)
SS.Yfit.A = sum(Yfit.A*Yfit.A)
SS.Yfit.B = sum(Yfit.B*Yfit.B)
if(!is.null(STI.mat)) SS.Yfit.AxB = sum(Yfit.AxB*Yfit.AxB)
MS.A=SS.Yfit.A/a
MS.B=SS.Yfit.B/b
if(!is.null(STI.mat)) MS.AxB=SS.Yfit.AxB/c
MS.Res=(SS.Y-SS.Yfit.ABAxB)/(n-(a+b+c)-1)


if(STI.test==TRUE) { #Test interaction (unrestricted permutations)
	nPGE.AxB=1
	Fref.AxB=MS.AxB/MS.Res
	vec=c(1:n)
	for(i in 1:nperm) {
		YPerm = Y[restrictedPerm(nobs.block=s,nblock=t, n,restPerm=0,vec),]
		YhatPerm.AxB = projAxB %*% YPerm
		SS.YhatPerm.AxB = sum(YhatPerm.AxB*YhatPerm.AxB)
		MS.Perm.AxB = SS.YhatPerm.AxB/c
	
		YhatPerm.ABAxB = projABAxB %*% YPerm
		SS.YhatPerm.ABAxB = sum(YhatPerm.ABAxB*YhatPerm.ABAxB)

		MS.Perm.Res = (SS.Y-SS.YhatPerm.ABAxB)/(n-(a+b+c)-1)

		Fper.AxB=MS.Perm.AxB/MS.Perm.Res
	 	if(Fper.AxB >= Fref.AxB) nPGE.AxB=nPGE.AxB+1
	}
	P.AxB=nPGE.AxB/(nperm+1)

	R2=SS.Yfit.AxB/SS.Y
	R2a=1-((n-1)/(n-dim(STI.mat)[2]-1))*(1-R2)
	
	testSTI = list(MS.num = MS.AxB, MS.den = MS.Res, R2 = R2, R2.adj = R2a,F=Fref.AxB, Prob=P.AxB)
} else {
	testSTI = NULL
}


if(S.test==TRUE) {#Test factor A (space) using restricted permutations within time blocks
	nPGE.A=1
	if(Tfixed==FALSE && !is.null(STI.mat)) {# Time random factor in crossed design with interaction
		Fref.A=MS.A/MS.AxB
		MS.den = MS.AxB
	} else if(Tfixed==FALSE && model=="6b") {# Time random factor in nested design
		Fref.A=MS.A/MS.B
		MS.den = MS.B
	} else {
		Fref.A=MS.A/MS.Res
		MS.den = MS.Res
	}
	vec=c(1:n)
	for(i in 1:nperm) {
		YPerm = Y[restrictedPerm(nobs.block=s,nblock=t, n,restPerm=1,vec),]
		YhatPerm.A = projA %*% YPerm
		SS.YhatPerm.A = sum(YhatPerm.A*YhatPerm.A)
		MS.Perm.A = SS.YhatPerm.A/a

		if(Tfixed==FALSE && !is.null(STI.mat)) { # Time random factor in crossed design with interaction
			YhatPerm.AxB = projAxB %*% YPerm
			SS.YhatPerm.AxB = sum(YhatPerm.AxB*YhatPerm.AxB)
			MS.Perm.AxB = SS.YhatPerm.AxB/c
			Fper.A=MS.Perm.A/MS.Perm.AxB
		} else if(Tfixed==FALSE && model=="6b") { # Time random factor in nested design
			YhatPerm.B = projB %*% YPerm
			SS.YhatPerm.B = sum(YhatPerm.B*YhatPerm.B)
			MS.Perm.B = SS.YhatPerm.B/b
			Fper.A=MS.Perm.A/MS.Perm.B
		} else {
			YhatPerm.ABAxB = projABAxB %*% YPerm
			SS.YhatPerm.ABAxB = sum(YhatPerm.ABAxB*YhatPerm.ABAxB)
			MS.Perm.Res = (SS.Y-SS.YhatPerm.ABAxB)/(n-(a+b+c)-1)
			Fper.A=MS.Perm.A/MS.Perm.Res
		}
		if(Fper.A >= Fref.A) nPGE.A=nPGE.A+1
	}
	P.A=nPGE.A/(nperm+1)

	R2=SS.Yfit.A/SS.Y
	R2a=1-((n-1)/(n-a-1))*(1-R2)
		
	testS = list(MS.num = MS.A, MS.den = MS.den, R2 = R2, R2.adj = R2a, F=Fref.A,Prob=P.A)
} else {
	testS = NULL
}

if(T.test==TRUE) {#Test factor B (time) using restricted permutations within time blocks
	nPGE.B=1
	if(Sfixed==FALSE && !is.null(STI.mat)) {# Space random factor in crossed design with interaction
		Fref.B=MS.B/MS.AxB
		MS.den = MS.AxB
	} else if(Sfixed==FALSE && model=="6a") {# Space random factor in nested design
		Fref.B=MS.B/MS.A
		MS.den = MS.A
	} else {
		Fref.B=MS.B/MS.Res
		MS.den = MS.Res
	}

	vec=c(1:n)
	for(i in 1:nperm) {
		YPerm = Y[restrictedPerm(nobs.block=s,nblock=t, n,restPerm=2,vec),]
		YhatPerm.B = projB %*% YPerm
		SS.YhatPerm.B = sum(YhatPerm.B*YhatPerm.B)
		MS.Perm.B = SS.YhatPerm.B/b

		if(Sfixed==FALSE && !is.null(STI.mat)) {# Space random factor in crossed design with interaction
			YhatPerm.AxB = projAxB %*% YPerm
			SS.YhatPerm.AxB = sum(YhatPerm.AxB*YhatPerm.AxB)
			MS.Perm.AxB = SS.YhatPerm.AxB/c
			Fper.B=MS.Perm.B/MS.Perm.AxB
		} else if(Sfixed==FALSE && model=="6a") {# Space random factor in nested design
			YhatPerm.A = projA %*% YPerm
			SS.YhatPerm.A = sum(YhatPerm.A*YhatPerm.A)
			MS.Perm.A = SS.YhatPerm.A/a
			Fper.B=MS.Perm.B/MS.Perm.A
		} else {
			YhatPerm.ABAxB = projABAxB %*% YPerm
			SS.YhatPerm.ABAxB = sum(YhatPerm.ABAxB*YhatPerm.ABAxB)
			MS.Perm.Res = (SS.Y-SS.YhatPerm.ABAxB)/(n-(a+b+c)-1)
			Fper.B=MS.Perm.B/MS.Perm.Res
		}	
		if(Fper.B >= Fref.B) nPGE.B=nPGE.B+1
	}
	P.B=nPGE.B/(nperm+1)

	R2=SS.Yfit.B/SS.Y
	R2a=1-((n-1)/(n-b-1))*(1-R2)
	
	testT = list(MS.num = MS.B, MS.den = MS.den, R2 = R2, R2.adj = R2a, F=Fref.B,Prob=P.B)
} else {
	testT = NULL
}

return(list(testSTI = testSTI, testS = testS, testT=testT))
}

'restrictedPerm' <- function(nobs.block, nblock, n, restPerm, vec)
#
# restPerm == 0: Unrestricted permutation. 
#
# restPerm == 1: Restricted permutation of the observations within each block.
# The data are arranged by blocks, with all observations forming a block 
# placed in adjacent positions in the file. Example:
# BLOCK-1: obs1, obs2, ... obs-nobs.block; BLOCK-2: obs1, obs2, ... obs-nobs.block; etc.
#
# restPerm == 2: Restricted permutation of the observations across blocks (within the blocks formed by each nth 
# observation).
#
# Vector 'vec' contains the initial order of the objects, e.g. vec=c(1:n).
# At the end of the function, it gives the permuted order of the objects.
#
# Examples:  toto0 <- restrictedPerm(6,4,24,0,c(1:24))
#            toto1 <- restrictedPerm(6,4,24,1,c(1:24))
#
#                                       Pierre Legendre, January 2006
#												   Miquel De Caceres, February 2009
{
	if(restPerm == 0) { 
		vec <- sample(vec[1:n],n)
	} else if(restPerm==1) {
		for(j in 1:nblock) {
			i1 <- nobs.block*(j-1)+1
			i2 <- nobs.block*j
			vec[i1:i2] <- sample(vec[i1:i2],nobs.block)
		}
	} else {
		vecT <- vec
		for(j in 1:nobs.block) {
			ind<-sample(1:nblock)
			for(i in 1:nblock) {
				vec[nobs.block*(i-1)+j]<-vecT[nobs.block*(ind[i]-1)+j]
			}
		}		
	}

	return(vec)
}