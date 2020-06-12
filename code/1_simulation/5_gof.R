##############################
rm(list=ls())
if(Sys.info()['user']=='s7m'){
	fPath = '~/Research/netsMatter/code/helpers/'
	dPath = '~/Dropbox/Research/netsMatter/'
	simResPath = paste0(dPath, 'simulation/')
	graphicsPath = paste('~/Research/netsMatter/paper/graphics/')
	source(paste0(fPath, 'functions.R')) }

if(Sys.info()['user']=='herme' | 'Owner'){
	user=Sys.info()['user']
	base = paste0('C:/Users/',user,'/')
	fPath = paste0(base, 'Research/netsMatter/code/helpers/')
	dPath = paste0(base, 'Dropbox/Research/netsMatter/')
	simResPath = paste0(dPath, 'simulation/')
	graphicsPath = paste(base, 'netsMatter/paper/graphics/')	
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
for(n in c( 50,100)){ load(paste0(simResPath,'ameSim',n,'.rda')) }

#
modKey = data.frame(dirty=names(ameSim50[[1]]$beta))
modKey$clean = c('Naive', 'AME', 'Oracle')
##############################

##############################
loadPkg('amen')
gofPlot(ameSim50[[1]]$gof$naive, FALSE)
gofPlot(ameSim50[[1]]$gof$ame, FALSE)
gofPlot(ameSim50[[1]]$gof$oracle, FALSE)
##############################
