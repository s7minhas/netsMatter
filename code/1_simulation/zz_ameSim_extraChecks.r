##############################
rm(list=ls())

if(Sys.info()['user']=='s7m'){
	fPath = '~/Research/netsMatter/code/helpers/'
	dPath = '~/Dropbox/Research/netsMatter/'
	simResPath = paste0(dPath, 'simulation/')
	source(paste0(fPath, 'functions.R')) }

if(Sys.info()['user']=='maxgallop'){
  fPath = '~/Documents/netsMatter/code/helpers/'
  dPath = '~/Dropbox/netsMatter/'
  simResPath = paste0(dPath, 'simulation/')
  source(paste0(fPath, 'functions.R')) }

if(Sys.info()['user']=='herme' | 'Owner'){
	user=Sys.info()['user']
	base = paste0('C:/Users/',user,'/')
	fPath = paste0(base, 'Research/netsMatter/code/helpers/')
	dPath = paste0(base, 'Dropbox/Research/netsMatter/')
	simResPath = paste0(dPath, 'simulation/')
	source(paste0(fPath, 'functions.R')) }

toLoad = c(
	'devtools',
	'foreach', 'doParallel',
	'magrittr', 'dplyr', 'ggplot2',
	'sna', # mrqap
	'sandwich', 'Matrix', 'lmtest', 'arm', 'reshape2' # asa
	)
loadPkg(toLoad)
# devtools::install_github('s7minhas/amen', ref='dev')
library(amen)
##############################

##############################
### Homophily
seed = 1
n = 30
intEff=-2 ; x1Eff=1 ; x2Eff=1
mu = intEff
beta = x1Eff
gamma = x2Eff
imps = 1000
imp=1
# simRun = function(seed, n, mu, beta, gamma){
	set.seed(seed)
	xw = matrix(rnorm(n*2),n,2)
	X = tcrossprod(xw[,1])  ; W = tcrossprod(xw[,2])
	XW = array(dim=c(n,n,2)) ; XW[,,1] = X ; XW[,,2] = W

	# create DV
	Y<- 1*(mu + beta*X  + matrix(rnorm(n*n),n,n) >0)

	# run AME models
	fit0 = ame(Y,X,R=0,rvar=FALSE,cvar=FALSE,dcor=FALSE,model='bin',
		print=FALSE,plot=FALSE)
	fit1 = ame(Y,X,R=1,rvar=FALSE,cvar=FALSE,dcor=FALSE,model='bin',
		print=FALSE,plot=FALSE)
	fitO = ame(Y,XW,R=0,rvar=FALSE,cvar=FALSE,dcor=FALSE,model='bin',
		print=FALSE,plot=FALSE)

	# MRQAP
	loadPkg('sna')
	fitQAP = netlm(y=Y, x=X,
		intercept=TRUE, mode='digraph', diag=FALSE,
		nullhyp='qap', reps=200)

	# aronow et al
	robust.se.nodfc <- function(model, cluster){
		M <- length(unique(cluster))
		N <- length(cluster)
		K <- model$rank
		dfc <- 1
		uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
		rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
		rcse.se <- coeftest(model, rcse.cov)
		return(list(rcse.cov, rcse.se)) }
	robust.se <- function(model, cluster){
		M <- length(unique(cluster))
		N <- length(cluster)
		K <- model$rank
		dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
		uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
		rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
		rcse.se <- coeftest(model, rcse.cov)
		return(list(rcse.cov, rcse.se)) }
	seHC <- sehat <- bhat <- matrix(NA, ncol=2, nrow=imps)
	y = Y ; diag(y) = NA ; rownames(y) = 1:n ; colnames(y) = 1:n
	dfSim = melt(y) ; dfSim$x = c(X) ; dfSim = na.omit(dfSim)
	fit <- lm(value~x, data=dfSim)
	bhat[imp,] <- coef(fit)
	# Dyadic cluster robust via multiway decomposition
	for(i in 1:n){
		iUp <- 1:n[i]
		clusUp <- apply(dfSim[,c('Var1','Var2')], 1,
						function(x){as.numeric(iUp %in% x)})
		clusIndexUp <- clusUp*-99 + (1-clusUp)*1:nrow(dfSim)
		if(i==1){dcrUp <- robust.se.nodfc(fit, clusIndexUp)[[1]]}
		if(i>1){dcrUp <- dcrUp + robust.se.nodfc(fit, clusIndexUp)[[1]]} }
	# substract naive CR:
	dataSim$dyads = with(dataSim, paste(Var1, Var2, sep='-')
	dcrUp2 <- dcrUp - robust.se.nodfc(fit, dataSim$dyads)[[1]]
	# substract HR:
	Vhat <- dcrUp2 - (length(1:n)-2)*vcovHC(fit,type="HC0")
	sehat[sim, ] <- sqrt(diag(Vhat))
	seHC[sim ,] <- sqrt(diag(vcovHC(fit, type="HC2")))
	seRE[sim, ] <- se.fixef(fit.re)

	# gather together results
	beta = list(naive=fit0$BETA, ame=fit1$BETA, oracle=fitO$BETA)
	qapBeta = matrix(fitQAP$coefficients,ncol=2,nrow=1)
	uv = list(naive=fit0$UVPM, ame=fit1$UVPM, oracle=fitO$UVPM)
	u = list(naive=fit0$U, ame=fit1$U, oracle=fitO$U)
	v = list(naive=fit0$V, ame=fit1$V, oracle=fitO$V)
	gof = list(naive=fit0$GOF, ame=fit1$GOF, oracle=fitO$GOF)
	out = list(
		beta=beta, uv=uv, u=u, v=v, W=W, gof=gof,
		qapBeta=qapBeta
		)
	return(out) }
##############################

##############################
# params
imps = 1000
intEff=-2 ; x1Eff=1 ; x2Eff=1

#
writeLines(c('n=50 using 8 cores\n'), paste0(simResPath,'ameSim50Log.txt'))
cl=makeCluster(8) ; registerDoParallel(cl)
ameSim50 = foreach(imp = 1:imps,
	.packages=c('amen','sna', 'sandwich', 'Matrix', 'lmtest','arm', 'reshape2')
	) %dopar% {
	out=simRun(seed=imp, n=50, mu=intEff, beta=x1Eff, gamma=x2Eff)
	sink(paste0(simResPath,'ameSim50Log.txt'), append=TRUE)
	cat(paste0(imp, ' out of ', imps, ' completed: ', getTime(),'\n'))
	return(out) } ; stopCluster(cl)
save( ameSim50, file=paste0(simResPath, 'ameSim50.rda') )

#
writeLines(c('n=100 using 8 cores\n'), paste0(simResPath,'ameSim100Log.txt'))
cl=makeCluster(8) ; registerDoParallel(cl)
ameSim100 = foreach(imp = 1:imps,
	.packages=c('amen','sna', 'sandwich', 'Matrix', 'lmtest','arm', 'reshape2')
	) %dopar% {
	out=simRun(seed=imp, n=100, mu=intEff, beta=x1Eff, gamma=x2Eff)
	sink(paste0(simResPath,'ameSim100Log.txt'), append=TRUE)
	cat(paste0(imp, ' out of ', imps, ' completed: ', getTime(),'\n'))
	return(out) } ; stopCluster(cl)
save( ameSim100, file=paste0(simResPath, 'ameSim100.rda') )


save(ameSim50, ameSim100, file=paste0(simResPath, 'ameSimNoW.rda'))
##############################
save(fit0, fit1, file = "noWsim.rda")
