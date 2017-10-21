# setup ###########################################
rm(list=ls())

if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	resultsPath = '~/Dropbox/Research/netsMatter/replications/McDonald_2004/data/'
	inputPath= '~/Dropbox/Research/netsMatter/replications/McDonald_2004/data/'
	plotPath = '~/Research/netsMatter/paper/' }

source('~/Research/netsMatter/code/helpers/functions.R')
source('~/Research/netsMatter/code/helpers/ameHelpers.R')
source('~/Research/netsMatter/code/helpers/binPerfHelpers.R')
############################################

# modData ###########################################
load(paste0(resultsPath,'McDonald_baseModel.rda'))
load( paste0(resultsPath,'ameFit_k2.rda') )
############################################

# coefSumm ###########################################
ameSumm = t(apply(ameFit$BETA, 2, function(x){ c(
	mu=mean(x),
	quantile(x, probs=c(0.025,0.05,0.95,0.975))) }))
############################################

# outPerf ###########################################
load(paste0(resultsPath, 'outsampResults2.rda')) # ameOutSamp_NULL
ameOutSamp=ameOutSamp_NULL ; rm(ameOutSamp_NULL)
ameOutSamp$outPerf$pred = 1/(1+exp(-ameOutSamp$outPerf$pred))
load(paste0(resultsPath,'McDGLMPerf.rda')) # glmOutSamp_wFullSpec
glmOutSamp=glmOutSamp_wFullSpec ; rm(glmOutSamp_wFullSpec)

# org
predDfs = list(
	GLM = data.frame(actual=glmOutSamp$outPerf$actual, pred=glmOutSamp$outPerf$pred, model='GLM'),
	AME = data.frame(actual=ameOutSamp$outPerf$actual, pred=ameOutSamp$outPerf$pred, model='AME') )

# run
ggPerfCurves(predDfs, 'mcdonald')
############################################

# abPlot ###########################################
effdat = getAddEffData(fit = ameFit) ##This function is in helperEx.R
effdat$actor = countrycode::countrycode(effdat$actor, 'cown', 'country.name')
effdat$actor = factor(effdat$actor,  levels=effdat[order(effdat$addEff),'actor'])

## All countries
addEffPlot(fit = effdat, addEffData = effdat, row = T)
############################################