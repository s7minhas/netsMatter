# setup ###########################################
rm(list=ls())

if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	resultsPath = '~/Dropbox/Research/netsMatter/replications/Reiter_Stam_2003/output/'
	inputPath= '~/Dropbox/Research/netsMatter/replications/Reiter_Stam_2003/input/'
	plotPath = '~/Research/netsMatter/paper/' }

source('~/Research/netsMatter/code/helpers/functions.R')
source('~/Research/netsMatter/code/helpers/ameHelpers.R')
source('~/Research/netsMatter/code/helpers/binPerfHelpers.R')
############################################

# modData ###########################################
load( paste0(resultsPath,'reiterStam_baseModel.rda') )
load( paste0(resultsPath,'ameFit_k2.rda') )
############################################

# coefSumm ###########################################
ameSumm = t(apply(ameFit$BETA, 2, function(x){ c(
	mu=mean(x),
	quantile(x, probs=c(0.025,0.05,0.95,0.975))) }))
############################################

# addeffdata ###########################################
effdat = getAddEffData(fit = ameFit) ##This function is in helperEx.R
effdat$actor = countrycode::countrycode(effdat$actor, 'cown', 'country.name')
effdat$actor = factor(effdat$actor,  levels=effdat[order(effdat$addEff),'actor'])

## All countries
addEffPlot(fit = effdat, addEffData = effdat, row = T)
############################################

# outPerf ###########################################
load(paste0(resultsPath, 'ameCrossValResults_k2.rda')) # ameOutSamp_k2
ameOutSamp=ameOutSamp_k2 ; rm(ameOutSamp_k2)
load(paste0(resultsPath,'glmCrossValResults.rda')) # glmOutSamp
glmOutSamp=glmOutSamp_wFullSpec ; rm(glmOutSamp_wFullSpec)
toKeep = which(!is.na(glmOutSamp$outPerf$pred))

# org
predDfs = list(
	GLM = data.frame(actual=glmOutSamp$outPerf$actual[toKeep], pred=glmOutSamp$outPerf$pred[toKeep], model='GLM'),
	AME = data.frame(actual=ameOutSamp$outPerf$actual, pred=ameOutSamp$outPerf$pred, model='AME') )

# run
ggPerfCurves(predDfs, 'reiter_stam')
############################################