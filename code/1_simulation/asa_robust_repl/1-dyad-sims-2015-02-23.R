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

robust.se <- function(model, cluster){
	require(sandwich)
	require(lmtest)
	M <- length(unique(cluster))
	N <- length(cluster)
	K <- model$rank
	dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
	uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
	rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
	rcse.se <- coeftest(model, rcse.cov)
	return(list(rcse.cov, rcse.se))
}

#################
# Global settings
#################

nsim <- 500  # To fix the number of simuluation runs


#################
# Cross-sectional
#################

# for(N in c(20,50,100,150)){
N = 50

index <- 1:N
P <- choose(N,2)

# Dyads

dyads <-  as.matrix(apply(t(combn(index, 2)), 1,
					function(x)paste(x[1],
					x[2],sep="-")))

dyad.mat <- t(apply(dyads, 1, function(x)unlist(strsplit(x,"-"))))

# Simulation

seRE <- bRE <- seHC <- sehat <- bhat <- matrix(NA, ncol=2, nrow=nsim)

# for(sim in 1:nsim){
sim=1
cat(paste("Sim #",sim,":",sep="")); flush.console()
# Generate the data

a <- rnorm(N) 
X <- rnorm(N)

dX <- da <- NA
for(i in 1:P){
da[i] <- sum(a[as.numeric(dyad.mat[i,])])
dX[i] <- abs(diff(X[as.numeric(dyad.mat[i,])]))
}

dY <- da + dX + rnorm(P)

dataUp <- data.frame(dyads,
					 dY, dX,
					 dyad1 = as.numeric(dyad.mat[,1]),
					 dyad2 = as.numeric(dyad.mat[,2]))

fit <- lm(dY~dX, data=dataUp)
bhat[sim,] <- coef(fit)

fit.re <- lmer(dY~dX + (1|dyad1) + (1|dyad2),data=dataUp)
bRE[sim,] <- fixef(fit.re)

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

sehat[sim, ] <- sqrt(diag(Vhat))
seHC[sim ,] <- sqrt(diag(vcovHC(fit, type="HC2")))
seRE[sim, ] <- se.fixef(fit.re)
write.out <- data.frame(bhat=bhat, sehat=sehat, seHC=seHC, bRE=bRE, seRE=seRE)
write.csv(write.out, file=paste("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-",
									N,".csv", 
									sep=""), row.names=F)
}

}

#################
# Repeated observations
#################

R <- 2

for(N in c(20,50,100,150)){
index <- 1:N
P <- choose(N,2)

# Dyads

dyads.sc <-  as.matrix(apply(t(combn(index, 2)), 1,
					function(x)paste(x[1],
					x[2],sep="-")))
dyads <- matrix(t(matrix(dyads.sc, ncol=R, nrow=nrow(dyads.sc))), ncol=1)
dyad.mat <- t(apply(dyads, 1, function(x)unlist(strsplit(x,"-"))))

# Simulation

seRE <- bRE <- seCR <- seHC <- sehat <- bhat <- matrix(NA, ncol=2, nrow=nsim)

for(sim in 1:nsim){
cat(paste("Sim #",sim,":",sep="")); flush.console()

a <- rnorm(N) 
X <- rnorm(N)

dX <- da <- NA
for(i in 1:nrow(dyad.mat)){
da[i] <- sum(a[as.numeric(dyad.mat[i,])])
dX[i] <- abs(diff(X[as.numeric(dyad.mat[i,])]))
}

dY <- da + dX + rnorm(P*R)

dataUp <- data.frame(dyads,
					 dY, dX,
					 dyad1 = as.numeric(dyad.mat[,1]),
					 dyad2 = as.numeric(dyad.mat[,2]))

fit <- lm(dY~dX, data=dataUp)
bhat[sim,] <- coef(fit)

fit.re <- lmer(dY~dX + (1|dyad1) + (1|dyad2) + (1|dyads),data=dataUp)
bRE[sim,] <- fixef(fit.re)


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

sehat[sim, ] <- sqrt(diag(Vhat))
seHC[sim ,] <- sqrt(diag(vcovHC(fit, type="HC2")))
seCR[sim,] <- sqrt(diag(robust.se(fit, dyads)[[1]]))
seRE[sim, ] <- se.fixef(fit.re)

write.out <- data.frame(bhat=bhat, sehat=sehat, seHC=seHC, seCR=seCR, bRE=bRE, seRE=seRE)
write.csv(write.out, file=paste("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-rep-",
									N,".csv", 
									sep=""), row.names=F)
}
}




