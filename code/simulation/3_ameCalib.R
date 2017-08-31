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
load(paste0(simResPath, 'ameSim30.rda')) ; load(paste0(simResPath, 'ameSim50.rda'))
load(paste0(simResPath, 'deprc/ameSim100.rda'))

#
modKey = data.frame(dirty=names(ameSim30[[1]]$beta))
modKey$clean = c('Naive', 'AME', 'Oracle')
##############################

getCoverage = function(ameSim, model, n, varName, varNum, actual){
	intSumm = data.frame( do.call('rbind', lapply(ameSim, function(x){
		quantile(x$beta[[model]][,varNum], probs=c(0.025, 0.975))
	}) ) )
	intSumm$coverage = ( intSumm[,1]<actual & actual<intSumm[,2] )
	intSumm$model = model
	intSumm$n = n
	intSumm$varName 
	return(intSumm)	
}


ameSimCover = lapply(c('naive','ame','oracle'), function(m){
	getCoverage(ameSim30, model=m, n=30, varName='X1', varNum=2, actual=x1Eff) })

ameSimCover = rbind(
	getCoverage(ameSim30, model='naive', n=30, varName='X1', varNum=2, actual=x1Eff),
	getCoverage(ameSim30, model='ame', n=30, varName='X1', varNum=2, actual=x1Eff),
	getCoverage(ameSim30, model='oracle', n=30, varName='X1', varNum=2, actual=x1Eff),
	)



FIT0_x1Eff_n30 = do.call('rbind',
	lapply(ameSim30, function(x){
		quantile(x$beta$'naive'[,2],probs=c(.025,.975))
	}) )

FIT1_x1Eff_n30 = do.call('rbind',
	lapply(ameSim30, function(x){
		quantile(x$beta$'ame'[,2],probs=c(.025,.975))
	}) )

FITO_x1Eff_n30 = do.call('rbind',
	lapply(ameSim30, function(x){
		quantile(x$beta$'oracle'[,2],probs=c(.025,.975))
	}) )

## coverage 
mean( FIT0_x1Eff_n30[,1]<x1Eff & x1Eff<FIT0_x1Eff_n30[,2] )
mean( FIT1_x1Eff_n30[,1]<x1Eff & x1Eff<FIT1_x1Eff_n30[,2] )
mean( FITO_x1Eff_n30[,1]<x1Eff & x1Eff<FITO_x1Eff_n30[,2] )

##############################
# check calibration
getCalibDF = function(ameSim){
	calibDF = lapply(ameSim, function(x){
		betaMod=lapply(x$beta, function(z){
			apply(z[,1:2], 2, function(y){
				qts = quantile(y,c(0.025, 0.975))

				qts[2] - qts[1]
			}) })
		betaMod = betaMod %>% reshape2::melt() %>%
			mutate(
				var=rep(c('Intercept','X1'), length(x$beta)),
				act = rep(c(intEff, x1Eff), length(x$beta)),
				) %>%
			rename(model = L1, width=value)
		return(betaMod) })
	return( suppressMessages( reshape2::melt(calibDF,id=names(calibDF[[1]])) ) ) }

#
ameSimCalib = rbind(
	cbind(getCalibDF(ameSim30), n=30),
	cbind(getCalibDF(ameSim30), n=50),
	cbind(getCalibDF(ameSim30), n=100) )

ameSimCalib %>% group_by(model, var, n) %>% summarise(widthAvg=mean(width))

ggplot(ameSimCalib, aes(x=model, y=width)) + 
	geom_boxplot() + 
	facet_grid(var ~ n, scales='free_y')
##############################