#############################
# set a path
require(here)
pth = paste0(here::here(), '/')
simResPath = pth

# install github from specific repo
# to get same results
# devtools::install_github('s7minhas/amen', ref='pa2018_version')
library(amen)

# source in helper functions
source(paste0(pth, '/helpers/functions.R'))

# load in additional pkgs
toLoad = c(
	'devtools',
	'foreach', 'doParallel',
	'magrittr', 'dplyr' )
loadPkg(toLoad)
##############################

##############################
### Homophily
simRun = function(seed, n, mu, beta, gamma){
	set.seed(seed)
	xw = matrix(rnorm(n*2),n,2)
	X = tcrossprod(xw[,1])  ; W = tcrossprod(xw[,2])
	XW = array(dim=c(n,n,2)) ; XW[,,1] = X ; XW[,,2] = W

	# create DV
	Y = 1*(mu + beta*X + gamma*W + matrix(rnorm(n*n),n,n) >0)

	# run AME models
	fit0 = ame(Y,X,R=0,rvar=FALSE,cvar=FALSE,dcor=FALSE,model='bin',
		print=FALSE,plot=FALSE)
	fit1 = ame(Y,X,R=1,rvar=FALSE,cvar=FALSE,dcor=FALSE,model='bin',
		print=FALSE,plot=FALSE)
	fitO = ame(Y,XW,R=0,rvar=FALSE,cvar=FALSE,dcor=FALSE,model='bin',
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
## running these simulations takes approximately
## 6 hours

# params
imps = 1000
intEff=-2 ; x1Eff=1 ; x2Eff=1

#
cl=makeCluster(30) ; registerDoParallel(cl)
ameSim50 = foreach(imp = 1:imps, .packages=c('amen')) %dopar% {
	out=simRun(seed=imp, n=50, mu=intEff, beta=x1Eff, gamma=x2Eff)
	return(out) } ; stopCluster(cl)
save( ameSim50, file=paste0(simResPath, 'ameSim50.rda') )

#
cl=makeCluster(30) ; registerDoParallel(cl)
ameSim100 = foreach(imp = 1:imps, .packages=c('amen')) %dopar% {
	out=simRun(seed=imp, n=100, mu=intEff, beta=x1Eff, gamma=x2Eff)
	return(out) } ; stopCluster(cl)
save( ameSim100, file=paste0(simResPath, 'ameSim100.rda') )
##############################
