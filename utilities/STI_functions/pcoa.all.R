'pcoa.all' <- function(D, diagonal=0, all=FALSE, include.zero=FALSE, rn=NULL)
# Principal coordinate decomposition of a square distance matrix D
# Get the eigenvectors corresponding to all eigenvalues, positive and negative
# Pierre Legendre, 2005, 2007
#
# D : A distance matrix of class 'dist' or 'matrix'.
# all : If TRUE, the eigenvectors corresponding to all eigenvalues, positive and negative, are shown in the output list.
# include.zero : If FALSE (default value), the zero eigenvalues as well as their eigenvectors are excluded from the output list.
# rn : An optional vector of row names, of length n, for the objects.
{
	epsilon <- sqrt(.Machine$double.eps) 
	D <- as.matrix(D)
	n <- nrow(D)
	D <- D + diag(rep(diagonal,n))

# Gower centring, matrix formula
	One <- matrix(1,n,n)
	mat <- diag(n) - One/n
	Dpr2 <- -0.5 * mat %*% (D^2) %*% mat
	trace <- sum(diag(Dpr2))

# Eigenvalue decomposition
	D.eig <- eigen(Dpr2, symmetric=TRUE)
	rel.values <- D.eig$values/trace
	rel.cum <- cumsum(rel.values)
	if(length(rn)!=0) {
		rownames(D.eig$vectors) <- rn
		} else {
		rownames(D.eig$vectors) <- rownames(D)
		}
	
# Output the results: k eigenvalues and eigenvectors
	if(all) {
		select <- 1:n
		if(!include.zero) {
			exclude <- which(abs(D.eig$values) < epsilon)
			select <- select[-exclude]
			}
		k <- length(select)
		res <- list(values=D.eig$values[select], rel.values=rel.values[select], rel.cum.values=rel.cum[select], vectors=D.eig$vectors[,select], trace=trace)
		# cat("k =",k,"Select =",select,'\n')

		} else {

		k <- length(which(D.eig$values > epsilon))		
		weight <- sqrt(D.eig$values[1:k])
		if(k == 1) {
			vectors <- D.eig$vectors[,1]*sqrt(D.eig$values[1])
			} else {
			vectors <- D.eig$vectors[,1:k]%*%diag(weight)
			}
		res <- list(values=D.eig$values[1:k], rel.values=rel.values[1:k], rel.cum.values=rel.cum[1:k], vectors=vectors, trace=trace)
		}
	res
}
