rm(list=ls())

if(Sys.info()['user']=='s7m'){
	fPath = '~/Research/netsMatter/code/helpers/'
	dPath = '~/Dropbox/Research/netsMatter/'
	simResPath = paste0(dPath, 'simulation/')
	source(paste0(fPath, 'functions.R'))
}

toLoad = c(
	'devtools', 
	'foreach', 'doParallel',
	'magrittr', 'dplyr', 'ggplot2'
	)
loadPkg(toLoad)
devtools::install_github('s7minhas/amen', ref='dev')

### Homophily
set.seed(6886)

simRun = function(NSIM, n, mu, beta, gamma, cores){

	# prlz
	cl = makeCluster(cores) ; registerDoParallel(cl)	
	simResults = foreach(sim = 1:NSIM, .packages=c('amen')) %dopar% {
		set.seed(sim) 
		xw = matrix(rnorm(n*2),n,2)
		X = tcrossprod(xw[,1]) 
		W = tcrossprod(xw[,2])
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

		out = list(beta=beta, uv=uv, u=u, v=v, gof=gof)
		return(out) }
	return(simResults) }

#
imps = 1000 ; cores = 7
intEff =  -2 ; x1Eff = 1 ; x2Eff = 1
ameSim30 = simRun(imps, 30, intEff, x1Eff, x2Eff, cores)
ameSim50 = simRun(imps, 30, intEff, x1Eff, x2Eff, cores)
ameSim100 = simRun(imps, 30, intEff, x1Eff, x2Eff, cores)

#
save(
	ameSim30, 
	ameSim50, 
	ameSim100, 
	file=paste0(simResPath, 'ameSim.rda')
	)
