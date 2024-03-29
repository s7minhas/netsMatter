##############################
rm(list=ls())

if(Sys.info()['user']=='s7m'){
	fPath = '~/Research/netsMatter/code/helpers/'
	dPath = '~/Dropbox/Research/netsMatter/'
	simResPath = paste0(dPath, 'simulation/')
	source(paste0(fPath, 'functions.R')) }

if(Sys.info()['user'] %in% c('herme','Owner','S7M')){
	base=paste0('C:/Users/',Sys.info()['user'],'/')
	gPath = paste0(base, 'Research/netsMatter/')
	fPath = paste0(gPath, 'code/helpers/')
	dPath = paste0(base, 'Dropbox/Research/netsMatter/')
	simResPath = paste0(dPath, 'simulation/')
	graphicsPath = paste(gPath, 'paper/graphics/')
	source(paste0(fPath, 'functions.R')) }

toLoad = c(
	'devtools',
	'foreach', 'doParallel',
	'magrittr', 'dplyr', 'ggplot2' )
loadPkg(toLoad)
library(amen)
##############################

##############################
### Homophily
simRun = function(seed, n, mu, beta, gamma, alpha){

	##############
	set.seed(seed)
	x=matrix(rnorm(n),n,1); X=tcrossprod(x[,1]); W = X^2
	XW = array(dim=c(n,n,2)) ; XW[,,1] = X ; XW[,,2] = W
	nodeEff = matrix(rnorm(n), nrow=n, ncol=n, byrow=FALSE)

	# create DV
	Y<- mu + beta*X + gamma*W + alpha*nodeEff + matrix(rnorm(n*n),n,n)
	Y<- 1*(Y >0)
	##############

	##############
	# run AME models
	fit0 = ame(
		Y,X,
		R=0,rvar=FALSE,cvar=FALSE,nvar=FALSE,symmetric=FALSE,model='bin',
		print=FALSE,plot=FALSE)

	fit1 = ame(
		Y,X,
		R=1,rvar=TRUE,cvar=TRUE,nvar=TRUE,symmetric=FALSE,model='bin',
		print=FALSE,plot=FALSE)

	fitO = ame(
		Y,XW,Xrow=nodeEff[,1,drop=FALSE],
		R=0,rvar=FALSE,cvar=FALSE,nvar=FALSE,symmetric=FALSE,model='bin',
		print=FALSE,plot=FALSE)
	##############

	# gather together results
	beta = list(naive=fit0$BETA, ame=fit1$BETA, oracle=fitO$BETA)
	uv = list(naive=fit0$UVPM, ame=fit1$UVPM, oracle=fitO$UVPM)
	u = list(naive=fit0$U, ame=fit1$U, oracle=fitO$U)
	v = list(naive=fit0$V, ame=fit1$V, oracle=fitO$V)
	gof = list(naive=fit0$GOF, ame=fit1$GOF, oracle=fitO$GOF)
	out = list(beta=beta, uv=uv, u=u, v=v, W=W, nodeEff=nodeEff, gof=gof)
	return(out) }
##############################

##############################
# params
imps = 100 ; cores = 30

#
cl=makeCluster(cores) ; registerDoParallel(cl)
ameSim50 = foreach(imp = 1:imps, .packages=c('amen')) %dopar% {
	out=simRun(seed=imp, n=50, mu=-2, beta=1, gamma=1, alpha=1)
	return(out) } ; stopCluster(cl)
save( ameSim50, file=paste0(simResPath, 'ameSim50_asaProbit.rda') )

#
cl=makeCluster(cores) ; registerDoParallel(cl)
ameSim100 = foreach(imp = 1:imps, .packages=c('amen')) %dopar% {
	out=simRun(seed=imp, n=100, mu=-2, beta=1, gamma=1, alpha=1)
	return(out) } ; stopCluster(cl)
save( ameSim100, file=paste0(simResPath, 'ameSim100_asaProbit.rda') )
##############################
