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
	'magrittr', 'dplyr', 'ggplot2' )
loadPkg(toLoad)
library(amen)
##############################

##############################
### Homophily
simRun = function(seed, n, mu, beta, gamma){

	##############
	# pull data gen code directly from asa
	index <- 1:n
	P <- choose(n,2)

	# Dyads
	dyads <-  as.matrix(
		apply(t(combn(index, 2)), 1,
			function(x)paste(x[1], x[2],sep="-")))

	dyad.mat <- t(apply(dyads, 1, function(x)unlist(strsplit(x,"-"))))

	set.seed(seed)
	m = function(x){matrix(x, ncol=2, nrow=1)}

	# gen some variables
	a <- rnorm(n)
	X <- rnorm(n)

	# Misspec
	dX <- da <- NA
	for(i in 1:P){
		da[i] <- sum(a[as.numeric(dyad.mat[i,])])
		dX[i] <- diff(X[as.numeric(dyad.mat[i,])])
	}

	# specify Y
	dX2 = dX^2
	dY <- 1 + mu*da + beta*dX + gamma*dX2 + rnorm(P)
	dY = 1*(dY>0)

	# create df
	dataUp <- data.frame(
		dyads, dY, da, dX, dX2,
		dyad1 = as.numeric(dyad.mat[,1]),
		dyad2 = as.numeric(dyad.mat[,2]),
		stringsAsFactors=FALSE
	)
	##############

	##############s
	### build data for ame
	## create adj mat representation of y and x
	actors = unique(c(dataUp$dyad1, dataUp$dyad2))
	n=length(actors)
	yMatrix = matrix(NA,
		nrow=n, ncol=n,
		dimnames=list(actors, actors) )
	xMatrix = array(NA,
		dim=c(n,n, 3),
		dimnames=list(actors, actors, c(
			'da', 'dX','dX2')) )

	# iterate through data and fill in adjMats
	for(iii in 1:nrow(dataUp)){
		# slice up data
		slice = dataUp[iii,,drop=FALSE]
		a1 = char(slice$dyad1) ; a2 = char(slice$dyad2)

		# fill in dv
		yMatrix[a1, a2] = slice$dY
		yMatrix[a2, a1] = slice$dY

		# fill in dyadic covariate
		xMatrix[a1,a2,'da'] = slice$da
		xMatrix[a2,a1,'da'] = slice$da
		xMatrix[a1,a2,'dX'] = slice$dX
		xMatrix[a2,a1,'dX'] = slice$dX
		xMatrix[a1,a2,'dX2'] = slice$dX2
		xMatrix[a2,a1,'dX2'] = slice$dX2
	}
	##############

	##############
	# run AME models
	params = list(
		list(0, 1, 0),
		list(
			xMatrix[,,'dX',drop=FALSE],
			xMatrix[,,'dX',drop=FALSE],
			xMatrix
		),
		list(FALSE, TRUE, FALSE)
	)

	fit0 = ame(
		yMatrix,xMatrix[,,'dX',drop=FALSE],
		R=0,rvar=FALSE,cvar=FALSE,nvar=FALSE,symmetric=TRUE,model='bin',
		print=FALSE,plot=FALSE)$BETA

	fit1 = ame(
		yMatrix,xMatrix[,,'dX',drop=FALSE],
		R=1,rvar=TRUE,cvar=TRUE,nvar=TRUE,symmetric=TRUE,model='bin',
		print=FALSE,plot=FALSE)$BETA

	fitO = ame(
		yMatrix,xMatrix,
		R=0,rvar=FALSE,cvar=FALSE,nvar=FALSE,symmetric=TRUE,model='bin',
		print=FALSE,plot=FALSE)$BETA
	##############

	# gather together results
	beta = list(naive=fit0$BETA, ame1=fit1$BETA, oracle=fitO$BETA)
	uv = list(naive=fit0$UVPM, ame=fit1$UVPM, oracle=fitO$UVPM)
	u = list(naive=fit0$U, ame=fit1$U, oracle=fitO$U)
	v = list(naive=fit0$V, ame=fit1$V, oracle=fitO$V)
	gof = list(naive=fit0$GOF, ame=fit1$GOF, oracle=fitO$GOF)
	out = list(beta=beta, uv=uv, u=u, v=v, W=W, gof=gof)
	return(out) }
##############################

##############################
# params
imps = 1000 ; cores = 30

#
cl=makeCluster(cores) ; registerDoParallel(cl)
ameSim50 = foreach(imp = 1:imps, .packages=c('amen')) %dopar% {
	out=simRun(seed=imp, n=50, gMu=1, mu=1, beta=1, gamma=.25)
	return(out) } ; stopCluster(cl)
save( ameSim50, file=paste0(simResPath, 'ameSim50_asa_probit.rda') )

#
cl=makeCluster(cores) ; registerDoParallel(cl)
ameSim100 = foreach(imp = 1:imps, .packages=c('amen')) %dopar% {
	out=simRun(seed=imp, n=100, gMu=1, mu=1, beta=1, gamma=.25)
	return(out) } ; stopCluster(cl)
save( ameSim100, file=paste0(simResPath, 'ameSim100_asa_probit.rda') )
##############################
