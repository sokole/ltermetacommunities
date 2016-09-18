TBI <- function(mat1,mat2,method="%difference", pa.tr=FALSE, nperm=99, permute.sp=1, BCD=TRUE, replace=FALSE, test.BC=TRUE, clock=FALSE)
{
### Internal functions
dissim <- function(mat1, mat2, n, method, tr=TRUE, BCD, ref)
# tr =TRUE : The species data have been transformed by decostand in transform()
# BCD=TRUE : Method is {"ruzicka", "%difference"} and output table BCD was requested
# ref=TRUE : The function is called to compute the reference values of the TBI dissimil.
{
vecD = vector(mode="numeric",length=n)     # to receive the values D=(B+C)/den
if(BCD) { 
	vecB = vector(mode="numeric",length=n) # to receive the values B/den
	vecC = vector(mode="numeric",length=n) # to receive the values C/den
	v.B  = vector(mode="numeric",length=n) # to receive the values B
	v.C  = vector(mode="numeric",length=n) # to receive the values C
	} else { vecB=NA; vecC=NA; v.B=NA; v.C=NA }
#
# Compute the dissimilarity between T1 and T2 for each object (site)
# 1. If method = {"hellinger", "chord"}, tr is TRUE
if(tr) for(i in 1:n) vecD[i] = dist(rbind(mat1[i,], mat2[i,]))
#
# 2. Compute the Euclidean distance
if(method == "euclidean")  
	for(i in 1:n) vecD[i] = dist(rbind(mat1[i,], mat2[i,])) 
# 3. Compute the Ruzicka or %difference dissimilarity 
# if(method == "ruzicka")          # Quantitative form of Jaccard
# if(method == "%difference")      # Quantitative form of Sørensen
if(any(method == c("ruzicka", "%difference"))) { 
	for(i in 1:n) {
		tmp = RuzickaD(mat1[i,], mat2[i,], method=method, BCD=BCD, ref=ref) 
		if(BCD) {
			vecB[i] <- tmp$B.den
			vecC[i] <- tmp$C.den
			v.B[i]  <- tmp$B
			v.C[i]  <- tmp$C    }
    	vecD[i] <- tmp$D
		}
	}
# Alternative method (not used here) to compute the %difference dissimilarity:
#	for(i in 1:n) vecD[i] = vegdist(rbind(mat1[i,], mat2[i,]), "bray")         # Slower
list(vecB=vecB, vecC=vecC, vecD=vecD, v.B=v.B, v.C=v.C)
}
###
transform <- function(mat, method)
{
if(method=="hellinger") mat = decostand(mat, "hellinger")
if(method=="chord")     mat = decostand(mat, "norm")
mat
}
### End internal functions

###
A <- system.time({
#
epsilon <- sqrt(.Machine$double.eps)
method <- match.arg(method, c("%difference", "ruzicka", "hellinger", "chord", "jaccard", "sorensen", "ochiai", "euclidean")) 
n = nrow(mat1)
p = ncol(mat1)
if((nrow(mat2)!=n) | (ncol(mat2)!=p)) stop("The matrices are not of the same size.")
#
if(method=="jaccard") { pa.tr=TRUE; method="ruzicka" }
if(method=="sorensen") { pa.tr=TRUE; method="%difference" }
if(method=="ochiai") { pa.tr=TRUE; method="hellinger" }
#
if(pa.tr) {
	mat1 <- ifelse(mat1>0, 1, 0)
	mat2 <- ifelse(mat2>0, 1, 0) }
if(any(method == c("hellinger", "chord"))) {
	tr <- TRUE
	require(vegan)
	} else { tr <- FALSE }
test.B.C <- NA 
if( (any(method == c("ruzicka", "%difference"))) & BCD) { 
	BCD.mat <- matrix(0,n,3)
	if(method=="%difference") colnames(BCD.mat) <- 
			c("B/(2A+B+C)","C/(2A+B+C)","D=(B+C)/(2A+B+C)")
	if(method=="ruzicka")    colnames(BCD.mat) <- 
			c("B/(A+B+C)","C/(A+B+C)","D=(B+C)/(A+B+C)")
	rownames(BCD.mat) <- paste("Site",1:n,sep=".")
	Change = vector(mode="character",length=n)
	} else {
	BCD <- FALSE 
	BCD.mat <- NA 
	BCD.summ <- NA 
	}
###
# 1. Compute the reference D for each object from corresponding vectors in the 2 matrices.
if(tr) { 
	tmp <- dissim(transform(mat1,method),transform(mat2,method),n,method,tr,BCD,ref=FALSE)
	} else { tmp <- dissim(mat1, mat2, n, method, tr, BCD, ref=TRUE) }
	vecD.ref <- tmp$vecD
	if(BCD) { BCD.mat[,1]<-tmp$vecB ; BCD.mat[,2]<-tmp$vecC ; BCD.mat[,3]<-tmp$vecD 
	for(i in 1:n) {
		if(tmp$vecB[i]>tmp$vecC[i]) Change[i]="–  " else Change[i]="+  " }
	BCD.summ = matrix(NA,1,6)
	colnames(BCD.summ) = c("mean(B/den)","mean(C/den)","mean(D)","B/(B+C)","C/(B+C)", 
		"Change")
	BCD.means = apply(BCD.mat,2,mean, na.rm=TRUE)  # Exclude the sites with value = NA
	BCD.summ[1,1:3] = BCD.means
	BCD.summ[1,4:5] = BCD.means[1:2]/BCD.means[3]
	BCD.summ = as.data.frame(BCD.summ)
	if(BCD.summ[1,1]>BCD.summ[1,2]) BCD.summ[1,6]="–  " else BCD.summ[1,6]="+  "
	rownames(BCD.summ) = ""
	#
	BCD.mat <- as.data.frame(BCD.mat)
	BCD.mat = cbind(BCD.mat,Change)
	#
	if((n>4) & test.BC) {   # Tests of significance of difference between B/den and C/den
		test.B.C = matrix(NA,2,3)
		rownames(test.B.C) = c("Paired t.test", "Wilcoxon test")
		# Paired t-test and Wilcokon test between the vectors of B and C values
		t.res = t.test(tmp$vecB, tmp$vecC, paired=TRUE, alternative = "two.sided")
		wilcox.res = wilcox.test(tmp$v.B, tmp$v.C, paired=TRUE, alternative="two.sided")
		test.B.C[1,] = c(t.res$estimate[[1]], t.res$statistic[[1]], t.res$p.value)
		test.B.C[2,2:3] = c(wilcox.res$statistic[[1]], wilcox.res$p.value)
		signif. = vector(mode="character",length=2)
		signif.[1] = ifelse(t.res$p.value>0.05, " ","*")
		signif.[2] = ifelse(wilcox.res$p.value>0.05, " ","*")
		test.B.C = as.data.frame(test.B.C)
		test.B.C = cbind(test.B.C, signif.)
		colnames(test.B.C) = c("  mean(B-C)","Stat","p.value","  p<=0.05")
		}
	# Matrix containing the observed values of B and C, in case they are needed later
	BC = cbind(tmp$v.B, tmp$v.C)
	colnames(BC) = c("B", "C")
	rownames(BC) <- paste("Site",1:n,sep=".")
	}
###
if(permute.sp!=3) {   # Permute the data separately in each column.
# 2. Permutation methods 1 and 2 --
# Permute *the raw data* by columns. Permute the two matrices in the same way, saving the seed before the two sets of permutations through sample(). 
# Permutation test for each distance in vector D
# seed: seed for random number generator, used by the permutation function 
#       sample(). It is reset to that same value before permuting the values in the  
#       columns of the second matrix. 
	if(nperm>0) {
		nGE.D = rep(1,n)
		for(iperm in 1:nperm) {
			BCD <- FALSE
			if(permute.sp==1) {    # Permutation method 1
				seed <- ceiling(runif(1,max=100000))
				# cat("seed =",seed,'\n')
				set.seed(seed)
				mat1.perm <- apply(mat1,2,sample,replace=replace)
				set.seed(seed)
				mat2.perm <- apply(mat2,2,sample,replace=replace)
			} else {  # Permutation method 2 - Do not force the permutations 
 					  # to start at the same point in the two matrices.
				mat1.perm <- apply(mat1,2,sample,replace=replace)
				mat2.perm <- apply(mat2,2,sample,replace=replace)
				}
# 3. Recompute transformations of the matrices and the D values of the paired vectors.
			if(tr) { tmp <- dissim(transform(mat1.perm,method), 
							transform(mat2.perm,method), n, method, tr, BCD, ref=FALSE)
			} else { tmp <- dissim(mat1.perm, mat2.perm, n, method, tr, BCD, ref=FALSE) }
			vecD.perm <- tmp$vecD
			ge <- which(vecD.perm+epsilon >= vecD.ref)
			if(length(ge)>0) nGE.D[ge] <- nGE.D[ge] + 1
			}
# 4. Compute the p-value associated with each distance.
		p.dist <- nGE.D/(nperm+1)
		} else { p.dist <- NA }   # if nperm=0

} else if(permute.sp==3) {   
# 2.bis  Permutation method 3 -- 
# Permute entire rows in each matrix separately.
	if(nperm>0) {
		seed <- ceiling(runif(1,max=100000))
		set.seed(seed)
		nGE.D = rep(1,n)
		for(iperm in 1:nperm) {
			BCD <- FALSE
			mat1.perm <- mat1[sample(n,replace=replace),]
			mat2.perm <- mat2[sample(n,replace=replace),]
			#
# 3.bis Recompute the D values of the paired vectors.
			if(tr) { tmp <- dissim(transform(mat1.perm,method), 
							transform(mat2.perm,method), n, method, tr, BCD, ref=FALSE)
			} else { tmp <- dissim(mat1.perm, mat2.perm, n, method, tr, BCD, ref=FALSE) }
			vecD.perm <- tmp$vecD
			ge <- which(vecD.perm+epsilon >= vecD.ref)
			if(length(ge)>0) nGE.D[ge] <- nGE.D[ge] + 1
			}
# 4.bis Compute the p-value associated with each distance.
		p.dist <- nGE.D/(nperm+1)
		} else { p.dist <- NA }   # if nperm=0
}
p.adj <- p.adjust(p.dist,"holm")
})
A[3] <- sprintf("%2f",A[3])
if(clock) cat("Computation time =",A[3]," sec",'\n')
#
list(TBI=vecD.ref, p.TBI=p.dist, p.adj=p.adj, BCD.mat=BCD.mat, BCD.summary=BCD.summ, test.B.C=test.B.C)
}

RuzickaD <- function(vec1, vec2, method="ruzicka", BCD=FALSE, ref=TRUE)
#
# Compute the Ruzicka dissimilarity (quantitative form of the Jaccard dissimilarity)
# or the percentage difference (quantitative form of the Sørensen dissimilarity).
# A single dissimilarity is computed because there are only two data vectors.
#
# Arguments --
# vec1, vec2 : data vectors (species abundance or presence-absence data)
# method == c("ruzicka", "%difference")
# BCD=TRUE  : Compute and save the B and C components of the %difference and Ruzicka D.
#             For the %difference, they are B/(2A+B+C), C/(2A+B+C), D/(2A+B+C).
#             For the Ruzicka D, they are B/(A+B+C), C/(A+B+C), D/(A+B+C).
# BCD=FALSE : Do not compute the components. BCD=FALSE for D other than %diff and Ruzicka.
# ref=TRUE  : Compute the reference values of D, B and C
#    =FALSE : Under permutation, compute only the value of D. Use separate code (shorter).
#
# License: GPL-2 
# Author:: Pierre Legendre, April 2015
{
# An algorithm applicable to matrices Y containing two data vectors only
#
A <- sum(pmin(vec1, vec2))          # A = sum of minima from comparison of the 2 vectors
sum.Y <- sum(vec1, vec2)            # Sum of all values in the two vectors, (2A+B+C)
#
if(ref) {    # Compute the reference values of statistics D, B and C
	tmp = vec1 - vec2
	B = sum(tmp[tmp>0])                 # Sum of the species losses between T1 and T2
	C = -sum(tmp[tmp<0])                # Sum of the species gains between T1 and T2
	D = B+C                             # Dissimilarity

	# Under permutation, compute only the value of D. - Shorter computation time.
	} else { 
	D <- sum.Y-2*A                      # (B+C)
	}
# Compute the denominator (den) of the Ruzicka or %difference index
if(method == "ruzicka") { den <-(sum.Y-A)  # den = (A+B+C)
	} else { den <- sum.Y }                # den = (2A+B+C)
if(!BCD) { B <- NA ; C <- NA }
list(B.den=B/den, C.den=C/den, D=D/den, B=B, C=C)
}

# Examples -- 
# data(mite)
# res1 = TBI(mite[1:10,],mite[61:70,],method="%diff",nperm=999,permute.sp=1)
# 
# Example using permute.sp=3. This method is not recommended (low power).
# res2 = TBI(mite[1:10,],mite[61:70,],method="hellinger",nperm=999,permute.sp=3)
