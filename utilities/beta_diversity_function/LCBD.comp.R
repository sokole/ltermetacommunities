LCBD.comp <- function(D, sqrt.D=TRUE, save.D=FALSE)
#
# Description --
#
# Compute LCBD indices (Legendre and De Cáceres 2013) from a dissimilarity 
# matrix (D) or beta div. component matrices (Repl, RichDiff or AbDiff, or Nes).
#
# Arguments --
#
# D : Dissimilarity or beta diversity component matrix, class=dist.
# sqrt.D : Take sqrt() of components before computing LCBD.comp. Use
#     sqrt.D=TRUE for the replacement and richness/abundance difference indices 
#     computed by beta.div.comp(), as well as for the corresponding D matrices.
# =>  When computing LCBD from a D matrix, use sqrt=TRUE if the D matrix is not
#     Euclidean. That property can be checked with function is.euclid() of ade4.
#
# Reference --
#
# Legendre, P. & De Cáceres, M. (2013) Beta diversity as the variance of 
# community data: dissimilarity coefficients and partitioning. Ecology 
# Letters 16: 951–963. 
#
# License: GPL-2 
# Author:: Pierre Legendre, August 2013
{
### Internal function   # Legendre & Legendre 2012, eq. 9.42
centre <- function(D,n)
   # Centre a square matrix D by matrix algebra
   # mat.cen = (I - 11'/n) D (I - 11'/n)
   {  One <- matrix(1,n,n)
      mat <- diag(n) - One/n
      mat.cen <- mat %*% D %*% mat
   }
###
D <- as.dist(D)
x <- as.matrix(D)
n <- nrow(x)

if(sqrt.D) {                          # D is used in Gower centring
   SStotal <- sum(D)/n
   BDtotal <- SStotal/(n-1)
   G <- centre(as.matrix(-0.5*x), n)
 } else {                             # D^2 is used in Gower centring
   SStotal <- sum(D^2)/n      # eq. 8
   BDtotal <- SStotal/(n-1)   # eq. 3
   G <- centre(as.matrix(-0.5*x^2), n)
   }
LCBD <- diag(G)/SStotal   # Legendre & De Caceres (2013), eq. 10b
if(save.D) {
out <- list(SStotal_BDtotal=c(SStotal,BDtotal), LCBD=LCBD, D=D)
} else {
out <- list(SStotal_BDtotal=c(SStotal,BDtotal), LCBD=LCBD) }
}