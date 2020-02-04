##############################
rm(list=ls())

if(Sys.info()['user']=='s7m'){
	fPath = '~/Research/netsMatter/code/helpers/'
	dPath = '~/Dropbox/Research/netsMatter/'
	simResPath = paste0(dPath, 'simulation/')
	source(paste0(fPath, 'functions.R')) }

toLoad = c(
	'devtools',
	'foreach', 'doParallel',
	'magrittr', 'dplyr', 'ggplot2' )
loadPkg(toLoad)
devtools::install_github('s7minhas/amen', ref='dev')
##############################

##############################
### Homophily
simRun = function(seed, n, mu, beta, gamma){
	set.seed(seed)
	xw = matrix(rnorm(n*2),n,2)
	X = tcrossprod(xw[,1])  ; W = tcrossprod(xw[,2])
	XW = array(dim=c(n,n,2)) ; XW[,,1] = X ; XW[,,2] = W

	# create DV
	Y<- 1*(mu + beta*X + gamma*W + matrix(rnorm(n*n),n,n) >0)

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
# params
imps = 1000
intEff=-2 ; x1Eff=1 ; x2Eff=1

#
getTime = function(){format(Sys.time(), '%X')}
writeLines(c('n=30 using 8 cores\n'), paste0(simResPath,'ameSim30Log.txt'))
cl=makeCluster(8) ; registerDoParallel(cl)
ameSim30 = foreach(imp = 1:imps, .packages=c('amen')) %dopar% {
	out=simRun(seed=imp, n=30, mu=intEff, beta=x1Eff, gamma=x2Eff)
	sink(paste0(simResPath,'ameSim30Log.txt'), append=TRUE)
	cat(paste0(imp, ' out of ', imps, ' completed: ', getTime(),'\n'))
	return(out) }  ; stopCluster(cl)
save( ameSim30, file=paste0(simResPath, 'ameSim30.rda') )

#
writeLines(c('n=50 using 8 cores\n'), paste0(simResPath,'ameSim50Log.txt'))
cl=makeCluster(8) ; registerDoParallel(cl)
ameSim50 = foreach(imp = 1:imps, .packages=c('amen')) %dopar% {
	out=simRun(seed=imp, n=50, mu=intEff, beta=x1Eff, gamma=x2Eff)
	sink(paste0(simResPath,'ameSim50Log.txt'), append=TRUE)
	cat(paste0(imp, ' out of ', imps, ' completed: ', getTime(),'\n'))
	return(out) } ; stopCluster(cl)
save( ameSim50, file=paste0(simResPath, 'ameSim50.rda') )

# #
# writeLines(c('n=100 using 8 cores\n'), paste0(simResPath,'ameSim100Log.txt'))
# cl=makeCluster(8) ; registerDoParallel(cl)
# ameSim100 = foreach(imp = 1:imps, .packages=c('amen')) %dopar% {
# 	out=simRun(seed=imp, n=100, mu=intEff, beta=x1Eff, gamma=x2Eff)
# 	sink(paste0(simResPath,'ameSim100Log.txt'), append=TRUE)
# 	cat(paste0(imp, ' out of ', imps, ' completed: ', getTime(),'\n'))
# 	return(out) } ; stopCluster(cl)
# save( ameSim100, file=paste0(simResPath, 'ameSim100.rda') )

#
# save(ameSim30, ameSim50, ameSim100, file=paste0(simResPath, 'ameSim.rda'))
##############################
