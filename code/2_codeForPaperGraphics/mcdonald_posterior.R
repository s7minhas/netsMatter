############################################
rm(list=ls())

if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	resultsPath = '~/Dropbox/Research/netsMatter/replications/McDonald_2004/data/'
	inputPath= '~/Dropbox/Research/netsMatter/replications/McDonald_2004/data/'
	plotPath = '~/Dropbox/Research/netsMatter/replications/0_finalRepFigs/' }

#
source('~/Research/netsMatter/code/helpers/functions.R')
source('~/Research/netsMatter/code/helpers/ameHelpers.R')

#
loadPkg(c('amen','countrycode'))
############################################

############################################
#
load(paste0(resultsPath,'McDonald_baseModel.rda'))
load( paste0(resultsPath,'ameFit_k2.rda') )
############################################

############################################
# coef summ
# paramPlot(ameFit$BETA[,1:12])
# paramPlot(ameFit$BETA[,12:23])

ameSumm = t(apply(ameFit$BETA, 2, function(x){ c(
	mu=mean(x),
	quantile(x, probs=c(0.025,0.05,0.95,0.975))) }))
# modSumm
############################################

############################################
#
effdat = getAddEffData(fit = ameFit) ##This function is in helperEx.R
effdat$actor = countrycode::countrycode(effdat$actor, 'cown', 'country.name')
effdat$actor = factor(effdat$actor,  levels=effdat[order(effdat$addEff),'actor'])

## All countries
addEffPlot(fit = effdat, addEffData = effdat, row = T)
############################################

############################################
#

############################################