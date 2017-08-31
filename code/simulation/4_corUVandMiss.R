##############################
rm(list=ls())
if(Sys.info()['user']=='s7m'){
	fPath = '~/Research/netsMatter/code/helpers/'
	dPath = '~/Dropbox/Research/netsMatter/'
	simResPath = paste0(dPath, 'simulation/')
	graphicsPath = paste('~/Research/netsMatter/paper/')
	source(paste0(fPath, 'functions.R')) }

toLoad = c(
	'devtools', 
	'foreach', 'doParallel',
	'magrittr', 'dplyr', 'ggplot2',
	'latex2exp', 'Cairo'
	)
loadPkg(toLoad)
facet_labeller = function(string){ TeX(string) }
##############################

##############################
# params
NSIM = 1000 ; intEff=-2 ; x1Eff=1 ; x2Eff=1

# load sim results
load(paste0(simResPath, 'ameSim30.rda'))
load(paste0(simResPath, 'ameSim50.rda'))
load(paste0(simResPath, 'deprc/ameSim100.rda'))

#
modKey = data.frame(dirty=names(ameSim30[[1]]$beta))
modKey$clean = c('Naive', 'AME', 'Oracle')
##############################

##############################
# check that uv covers missvar
# missing var
genMissVar = function(n, imps=NSIM){
	lapply(1:imps, function(seed){ n=n ; 
		set.seed(seed) ; xw = matrix(rnorm(n*2),n,2)
		W = tcrossprod(xw[,2]) ; return(W) }) }
W30 = genMissVar(30)
W50 = genMissVar(50)
W100 = genMissVar(100, imps=NSIM/10)


##############################