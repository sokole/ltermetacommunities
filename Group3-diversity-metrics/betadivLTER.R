
betadivLTER <- function(Y, s=s, t=t, plot=TRUE)
# Thomas LAMY
# See Legendre, P. 2014. Interpreting the replacement and richness difference components of beta diversity. Global Ecology and Biogeography 23: 1324-1334.
# add some bla bla
# Based on BRAY-CURTIS / quantitative Sorensen <= to discuss
{
## Dependancies
	## -- the following functions are required and need to be loaded
source("./utilities/beta_diversity_function/beta.div.comp.R")

	## computation for each time step (spatial beta diverity)
	T = rep(1,s)
	for(i in 2:t) T = c(T,rep(i,s))
	T = as.numeric(T)
	beta.space <- data.frame()
	## loop over each time step
	for (i in 1:length(unique(T))){
		Y.t <- Y[ T==i ,]

		## compute total beta diversity and its two components
		rest = beta.div.comp(Y.t, coef="S", quant=TRUE, save.abc=FALSE)
		## formate result table
		beta.space = rbind(beta.space, data.frame(time=i, 
			BDtotal=rest$part[1],
			repl=rest$part[2],
			rich=rest$part[3],
			replcont=rest$part[4],
			richcont=rest$part[5]))
	}

	## computation for each site (temporal beta diverity)
	S = as.factor(rep(1:s,t))
	S = as.numeric(S)
	beta.time <- data.frame()
	## loop over each spatial unit
	for (i in 1:length(unique(S))){
		Y.t <- Y[ S==i ,]
		rest = beta.div.comp(Y.t, coef="S", quant=TRUE, save.abc=FALSE)
		beta.time = rbind(beta.time, data.frame(site=i, 
			BDtotal=rest$part[1],
			repl=rest$part[2],
			rich=rest$part[3],
			replcont=rest$part[4],
			richcont=rest$part[5]))
	}

	if(plot==TRUE){
		par(mfrow=c(1,2))
		barplot(t(as.matrix(beta.space[,3:4])), col=c("red", "green"), ylab="Beta diversity", xlab="Time", ylim=c(0,0.6), main="SPACE")
		barplot(t(as.matrix(beta.time[,3:4])), col=c("red", "green"), ylab="Beta diversity", xlab="Site", ylim=c(0,0.6), main="TIME")
		#plot(beta.space$time, beta.space$BDtotal, type="b", lwd=2, ylab="Beta diversity", xlab="Time", ylim=c(0,0.6), main="SPACE")
		#lines(beta.space$time, beta.space$repl, type="b", lwd=2, col="red")
		#lines(beta.space$time, beta.space$rich, type="b", lwd=2, col="green")
	}
 
 	out <- list(time=beta.time, space=beta.space)
 	out
}

