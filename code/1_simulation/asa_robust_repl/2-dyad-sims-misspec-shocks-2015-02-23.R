library(sandwich)
library(lmtest)
library(arm)

rm(list=ls())


# Cluster robust variance estimation function
robust.se.nodfc <- function(model, cluster){
	require(sandwich)
	require(lmtest)
	M <- length(unique(cluster))
	N <- length(cluster)
	K <- model$rank
	dfc <- 1
	uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
	rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
	rcse.se <- coeftest(model, rcse.cov)
	return(list(rcse.cov, rcse.se))
}


#################
# Global settings
#################

nsim <- 500

#################
# Cross-sectional: misspecification
#################

#N <- 150

for(N in c(20,50,100,150)){

index <- 1:N
P <- choose(N,2)

# Dyads

dyads <-  as.matrix(apply(t(combn(index, 2)), 1,
					function(x)paste(x[1],
					x[2],sep="-")))

dyad.mat <- t(apply(dyads, 1, function(x)unlist(strsplit(x,"-"))))

# Simulation

bre.irreg <- seRE.irreg <- seHC.irreg <- sehat.irreg <- bhat.irreg <- matrix(NA, ncol=2, nrow=nsim)

for(sim in 1:nsim){
cat(paste("Sim #",sim,":",sep="")); flush.console()
# Generate the data

a <- rnorm(N)
X <- rnorm(N)

# Misspec

dX <- da <- NA
for(i in 1:P){
da[i] <- sum(a[as.numeric(dyad.mat[i,])])
dX[i] <- abs(diff(X[as.numeric(dyad.mat[i,])]))
}

dY <- da + dX + .25*dX^2 + rnorm(P)

dataUp <- data.frame(dyads,
					 dY, dX,
					 dyad1 = as.numeric(dyad.mat[,1]),
					 dyad2 = as.numeric(dyad.mat[,2]))


fit <- lm(dY~dX, data=dataUp)
bhat.irreg[sim,] <- coef(fit)

fit.re <- lmer(dY~dX + (1|dyad1) + (1|dyad2),data=dataUp)
bre.irreg[sim,] <- fixef(fit.re)

# Dyadic cluster robust via multiway decomposition
for(i in 1:length(index)){
	iUp <- index[i]
	clusUp <- apply(dyad.mat, 
					1,
					function(x)as.numeric(iUp %in% x))
	clusIndexUp <- clusUp*-99 + (1-clusUp)*1:nrow(dyad.mat)
	if(i==1){dcrUp <- robust.se.nodfc(fit, clusIndexUp)[[1]]}
	if(i>1){dcrUp <- dcrUp + robust.se.nodfc(fit, clusIndexUp)[[1]]}	
	cat(paste(iUp, " ")); flush.console()
}
# substract naive CR:
dcrUp2 <- dcrUp - robust.se.nodfc(fit, dataUp$dyads)[[1]]
# substract HR:
Vhat <- dcrUp2 - (length(index)-2)*vcovHC(fit,type="HC0")
sehat.irreg[sim, ] <- sqrt(diag(Vhat))
seHC.irreg[sim, ] <- sqrt(diag(vcovHC(fit, type="HC2")))
seRE.irreg[sim, ] <- se.fixef(fit.re)

write.out <- data.frame(	bhat.irreg=bhat.irreg, 
							sehat.irreg=sehat.irreg,
							seHC.irreg=seHC.irreg,
							bre.irreg = bre.irreg, 
							seRE.irreg=seRE.irreg)
write.csv(write.out, file=paste("~/Dropbox/dyadic-variance/sims/simout-misspec-",
									N,".csv", 
									sep=""), row.names=F)

}

}
