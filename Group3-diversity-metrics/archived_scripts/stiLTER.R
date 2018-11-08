varpartSTI <- function(Y, s=s, t=t)
# Thomas LAMY
# September 19th 2016
# add some bla bla
# 
{
	## Dependancies
	## -- the following functions are required and need to be loaded
	source("./utilities/STI_functions/PCNM.R")
	source("./utilities/STI_functions/pcoa.all.R")
	source("./utilities/STI_functions/manovRDa.R")

	# Center response data (by observation mean --> Check this!!!)
	Y = apply(Y,2,scale,center=TRUE,scale=FALSE)

	## Generate space and time helmert contrasts
	S = as.factor(rep(1:s,t))
	T = rep(1,s)
	for(i in 2:t) T = c(T,rep(i,s))
	T = as.factor(T)
	HM = model.matrix(~ S + T, contrasts = list(S="contr.helmert", T="contr.helmert"))
	XS = as.matrix(HM[,2:s])
	XT = as.matrix(HM[,(s+1):(s+t-1)])

	## Generate PCNM space variables
	sitesX <- c(1:s)
	## Keeping s/2 PCNM space variables
	nS = trunc(s/2)
	## running PCNM function on the "distance" between sites (as factors)
	sitesX.D=dist(sitesX)
	pA = PCNM(sitesX.D, all=TRUE, include.zero=TRUE)
	## Compute how many positive eigenvalues
	npos = 0
	for(i in 1:length(pA$values)) if(pA$values[i]>0) npos=npos+1
	## Do not get the eigenvector corresponding to the zero eigenvalue
	SS = as.matrix(pA$vectors[,-(npos+1)][,1:nS])
	PCNM.S = SS
	## formating blocks of PCNM space variables for the final dataset 
	for(j in 2:t) PCNM.S = rbind(PCNM.S,SS)

	## Generate PCNM time variables
	timesX = c(1:t)
	## Keeping t/2 PCNM time variables
	nT = trunc(t/2)
	## running PCNM function on the "distance" between years
	timesX.D = dist(timesX)
	pB = PCNM(timesX.D, all=TRUE, include.zero=TRUE)
	## Compute how many positive eigenvalues
	npos = 0		
	for(i in 1:length(pB$values)) if(pB$values[i]>0) npos=npos+1
	## Do not get the eigenvector corresponding to the zero eigenvalue
	TT = as.matrix(as.matrix(pB$vectors[,-(npos+1)])[,1:nT])
	## Matching PCNM time variables with blocks of sites 
	T.temp = TT[1,]
	for(i in 2:s) T.temp = rbind(T.temp,TT[1,])
	## formating blocks of PCNM time variables for the final dataset 
	PCNM.T = as.matrix(T.temp)
	for(j in 2:t){
		T.temp = TT[j,]
		for(i in 2:s) T.temp = rbind(T.temp,TT[j,])
				PCNM.T = as.matrix(rbind(PCNM.T,T.temp))
	}

	## generate the space-time interaction factors out of the PCNM variables
	## by multiplying each spatial PCNM variable by each temporal PCNM variable
	XSTI = PCNM.S*PCNM.T[,1]
	if(dim(PCNM.T)[2]>1) for(j in 2:dim(PCNM.T)[2]) XSTI = cbind(XSTI,PCNM.S*PCNM.T[,j])

	## Computing fractions
	## Option 1, using varpart function: may not be appropriate: to check!
	## var1 <- varpart(Y, XS, XT, XSTI)

	## option 2, using Two way MANOVA models to test space-time interaction without replicates
	## see function manovRDa Pierre Legendre & Miquel De Caceres
	res <- manovRDa(Y=Y,s=s,t=t,S.mat=XS,T.mat=XT,STI.mat=XSTI, Sfixed=TRUE, Tfixed=TRUE, S.test=TRUE, T.test=TRUE, STI.test=TRUE, model = "5", nperm=999)

	out <- data.frame(Space=res$testS$R2, pSpace=res$testS$Prob, Time=res$testT$R2, pTime=res$testT$Prob, STI=res$testSTI$R2, pSTI=res$testSTI$Prob)
	out
}


