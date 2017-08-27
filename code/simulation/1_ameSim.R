rm(list=ls())
library(statnet)
library(latentnet)
library(ergm)
# install.packages('amen')
library(amen)
library(foreach)
library(doParallel)

### Simulate dataset with intercept val of 1 and dyadvar val of 2
set.seed(6886)
n = 30
sims = 100
cores = 100

cl = makeCluster(cores) ; registerDoParallel(cl)
simResults = foreach(i = 1:sims, .packages=c('amen','statnet','latentnet','ergm')) %dopar% {
	edgeVar = matrix(rnorm(n * n, 0, 1), nrow=n, ncol=n)
	nw = simulate.formula(
	  network(n) ~ edges + edgecov(edgeVar) + idegree(1) + odegree(1) + mutual + balance, 
	  coef=c(-3, .5, .2, .2, .3, .05))

	par(mfrow=c(1,1))
	plot(nw)

	### run ergm
	# ergmFit = ergm(nw~edges + edgecov(edgeVar) + balance)

	## run lsm
	# lsmEuclFit = ergmm(nw ~ euclidean(2) + edgecov(edgeVar))
	# lsmBilFit = ergmm(nw ~ bilinear(2) + edgecov(edgeVar))

	### run ame
	nwAdj = as.matrix.network(nw) ; diag(edgeVar) = NA
	xArr = array(edgeVar, dim=c(dim(edgeVar)[1:2], 1))
	ameFit_k0 = ame(Y=nwAdj, Xdyad = xArr,
	                symmetric=FALSE, model='bin',
	                intercept=TRUE, R=0, 
	                # rvar=FALSE, cvar=FALSE, dcor=FALSE, nvar=FALSE,
	                seed=6886, plot=FALSE, print=FALSE
	                # ,nscan=50000, burn=25000, odens=50
	                )
	ameFit_k2 = ame(Y=nwAdj, Xdyad = xArr, 
	                symmetric=FALSE, model='bin',
	                intercept=TRUE, R=2,
	                # rvar=FALSE, cvar=FALSE, dcor=FALSE, nvar=FALSE,
	                seed=6886, plot=FALSE, print=FALSE
	                # ,nscan=50000, burn=25000, odens=50
	                )    

	### run glm for fun
	glmLogitFit = glm(c(nwAdj)~c(xArr), family = binomial(link = "logit"))
	glmProbitFit = glm(c(nwAdj)~c(xArr), family = binomial(link = "probit"))
	logProbFac = coef(glmLogitFit)/coef(glmProbitFit)

	### combo results
	coefSumm=rbind(
	  glmLog=coef(glmLogitFit), 
	  glmProb=coef(glmProbitFit),
	  # ergm=coef(ergmFit)[1:2],
	  # lsmEucl=summary(lsmEuclFit)$pmean$coef.table[,1],
	  # lsmBil=summary(lsmBilFit)$pmean$coef.table[,1],    
	  amen_k0=apply(ameFit_k0$BETA, 2, mean)*1.7,
	  amen_k2=apply(ameFit_k2$BETA, 2, mean)*1.7
	)

	plot(ameFit_k2)
	return(coefSumm)
}

#
