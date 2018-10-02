library(foreign)
library(sandwich)
library(Matrix)

rm(list=ls())

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


setwd("~/Research/netsMatter/code/1_simulation/asa_robust_repl/")

sdat.sc <- read.csv("Speed-Dating-Data.csv")
sdat.sc <- subset(sdat.sc, !is.na(iid))
sdat.sc <- subset(sdat.sc, gender==0) # include only data on females, 
									  # since replicating Col 1, Table III
sdat <- na.omit(sdat.sc[,c("pid","dec","iid",
				"attr","sinc","intel","fun","amb","shar")])

gendat <- rep(1,nrow(sdat))
wts <- ave(gendat,sdat$iid,FUN=sum)
fit <- lm(dec~amb+attr+intel +factor(iid), 
				data=sdat, weights = 1/wts,
				x=T)

# Multiway decomposition

sdat$fid <- 1+sdat$iid*.001
sdat$mid <- 2+sdat$pid*.001
index <- unique(c(sdat$fid, sdat$mid))
dyad.mat <- cbind(as.character(sdat$fid),as.character(sdat$mid))
sdat$dyadid <- apply(dyad.mat, 1, function(x)paste(x[1],x[2],sep="-"))

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
dcrUp2 <- dcrUp - robust.se.nodfc(fit, sdat$dyadid)[[1]]
# substract HR:
Vhat <- dcrUp2 - (length(index)-2)*vcovHC(fit,type="HC0")

coef(fit)[2:4]
sqrt(diag(vcovHC(fit,type="HC0")))[2:4]
sqrt(diag(robust.se.nodfc(fit, sdat$iid)[[1]][2:4,2:4]))
sqrt(diag(Vhat)[2:4])
