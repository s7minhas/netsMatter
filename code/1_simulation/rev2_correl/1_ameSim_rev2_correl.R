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
	graphicsPath = paste(gPath, 'paper/')
	source(paste0(fPath, 'functions.R')) }

toLoad = c(
	'devtools',
	'foreach', 'doParallel',
	'magrittr', 'dplyr', 'ggplot2',
  'amen' )
loadPkg(toLoad)
##############################

##############################
### Homophily
simRun = function(seed, n, mu, beta, gamma, rho){

  # cor mat
  theta = asin(rho)/2
  C = matrix( c(cos(theta),sin(theta),sin(theta),cos(theta)),2,2)

  # seed
  set.seed(seed)

  # gen covars
  xw = matrix(rnorm(n*2),n,2) %*% C
  X = tcrossprod(xw[,1])
  W = tcrossprod(xw[,2])
  XW = array(dim=c(n,n,2)) ; XW[,,1]<-X ; XW[,,2]<-W

	# create DV
	Y = mu + beta*X + gamma*W + matrix(rnorm(n*n),n,n)

	# run AME models
	fit0 = ame(Y,X,R=0,rvar=FALSE,cvar=FALSE,dcor=FALSE,
    model='nrm',
		print=FALSE,plot=FALSE)
	fit1 = ame(Y,X,R=1,rvar=FALSE,cvar=FALSE,dcor=FALSE,
    model='nrm',
		print=FALSE,plot=FALSE)
	fitO = ame(Y,XW,R=0,rvar=FALSE,cvar=FALSE,dcor=FALSE,
    model='nrm',
		print=FALSE,plot=FALSE)

	# gather together results
	beta = list(naive=fit0$BETA, ame=fit1$BETA, oracle=fitO$BETA)
	uv = list(naive=fit0$UVPM, ame=fit1$UVPM, oracle=fitO$UVPM)
	u = list(naive=fit0$U, ame=fit1$U, oracle=fitO$U)
	v = list(naive=fit0$V, ame=fit1$V, oracle=fitO$V)
	gof = list(naive=fit0$GOF, ame=fit1$GOF, oracle=fitO$GOF)
	out = list(beta=beta, uv=uv, u=u, v=v, W=W, gof=gof)
	return(out) }
##############################

##############################
# params
imps = 1000
cores = 30
intEff=-2 ; x1Eff=1 ; x2Eff=1

###############
#
cl=makeCluster(cores)  ; registerDoParallel(cl)
ameSim50 = foreach(imp = 1:imps, .packages=c('amen')) %dopar% {
	out=simRun(
    seed=imp, n=50, mu=intEff, beta=x1Eff, gamma=x2Eff, rho=.4)
	return(out) } ; stopCluster(cl)
save( ameSim50, file=paste0(simResPath, 'ameSim50_corrMed.rda') )

#
cl=makeCluster(cores)  ; registerDoParallel(cl)
ameSim100 = foreach(imp = 1:imps, .packages=c('amen')) %dopar% {
	out=simRun(
    seed=imp, n=100, mu=intEff, beta=x1Eff, gamma=x2Eff, rho=.4)
	return(out) } ; stopCluster(cl)
save( ameSim100, file=paste0(simResPath, 'ameSim100_corrMed.rda') )
###############

###############
#
cl=makeCluster(cores)  ; registerDoParallel(cl)
ameSim50 = foreach(imp = 1:imps, .packages=c('amen')) %dopar% {
	out=simRun(
    seed=imp, n=50, mu=intEff, beta=x1Eff, gamma=x2Eff, rho=.7)
	return(out) } ; stopCluster(cl)
save( ameSim50, file=paste0(simResPath, 'ameSim50_corrHi.rda') )

#
cl=makeCluster(cores)  ; registerDoParallel(cl)
ameSim100 = foreach(imp = 1:imps, .packages=c('amen')) %dopar% {
	out=simRun(
    seed=imp, n=100, mu=intEff, beta=x1Eff, gamma=x2Eff, rho=.7)
	return(out) } ; stopCluster(cl)
save( ameSim100, file=paste0(simResPath, 'ameSim100_corrHi.rda') )
###############
##############################
