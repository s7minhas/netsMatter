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

# Params
n = 50 # sample size
mu =  -2 # intercept eff
beta = 1 # x1 eff
gamma = 2 # x2 eff

FIT0 = FIT1 = FITO = NULL

# for(sim in 1:NSIM)
# { 
sim = 1 
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
  FIT0 = rbind(FIT0,quantile(fit0$BETA[,2],probs=c(.025,.5,.975))) 
  FIT1 = rbind(FIT1,quantile(fit1$BETA[,2],probs=c(.025,.5,.975)))  
  FITO = rbind(FITO,quantile(fitO$BETA[,2],probs=c(.025,.5,.975)))  
  cat(sim,"\n") 
}