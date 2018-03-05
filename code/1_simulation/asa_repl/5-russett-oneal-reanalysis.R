library(foreign)
library(sandwich)
library(Matrix)
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

triangle <- read.dta("~/Dropbox/dyadic-variance/pa-submission/replication-files/TRIANGLE-out.dta")
regSpec <- paste("dispute1~", 
				paste("allies","lcaprat2","smldmat","smldep",
					"smigoabi","noncontg","logdstab","minrpwrs",
				sep="+"), sep="")

regForm <- as.formula(regSpec)
fit <- glm(regForm, data=triangle, x=T, family="binomial")

index.sc <- unique(c(triangle$statea, triangle$stateb))
index <- index.sc[order(index.sc)]
dyad.mat <- triangle[,c("statea","stateb")]

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
dcrUp2 <- dcrUp - robust.se.nodfc(fit, triangle$dyadid)[[1]]
# substract HR:
Vhat <- dcrUp2 - (length(index)-2)*vcovHC(fit,type="HC0")
sqrt(diag(Vhat))

abs(coef(fit)/sqrt(diag(Vhat))) < 1.64







