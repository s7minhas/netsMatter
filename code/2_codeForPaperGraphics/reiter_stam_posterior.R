# setup ###########################################
rm(list=ls())

if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	resultsPath = '~/Dropbox/Research/netsMatter/replications/Reiter_Stam_2003/output/'
	inputPath= '~/Dropbox/Research/netsMatter/replications/Reiter_Stam_2003/input/'
	plotPath = '~/Research/netsMatter/paper/' }

source('~/Research/netsMatter/code/helpers/functions.R')
source('~/Research/netsMatter/code/helpers/ameHelpers.R')
source('~/Research/netsMatter/code/helpers/binPerfHelpers.R')
source('~/Research/netsMatter/code/helpers/stargazerHelpers.R')
############################################

# modData ###########################################
load( paste0(resultsPath,'reiter_stam_glmfit.rda') )
load( paste0(resultsPath,'model_k2_v12.rda') )
############################################

# coefSumm ###########################################
ameSumm = t(apply(ameFit$BETA, 2, function(x){ c(
	'Estimate'=mean(x), 'Std. Error'=sd(x), 'z value'=mean(x)/sd(x),
	'Pr(>|z|)'=2*(1-pnorm( abs(mean(x)/sd(x)))) )}))
rownames(ameSumm) = gsub('.row','',rownames(ameSumm),fixed=TRUE)
rownames(ameSumm) = gsub('.col','',rownames(ameSumm),fixed=TRUE)
rownames(ameSumm) = gsub('.dyad','',rownames(ameSumm),fixed=TRUE)
rownames(ameSumm) = gsub('_s','s',rownames(ameSumm),fixed=TRUE)
rownames(ameSumm) = gsub('intercept','(Intercept)',rownames(ameSumm),fixed=TRUE)
glmSumm = modSumm[,]
probSumm = proSumm[,]

modList = list(glmSumm, probSumm, ameSumm)
modNames = c('GLM (Logit)','GLM (Probit)', 'AME')

varKey = data.frame(
	dirty=names(coef(mod)),
	clean=c('Intercept', 'Pers/Democ Directed Dyad',
		'Democ/Pers Directed Dyad',
        'Personal', 'Military', 'Single', 'Democracy', 
        'Contiguous', 'Major Power', 'Ally', 'Higher/Lower Power Ratio', 
        'Economically Advanced', 'Years Since Last Dispute', 'Cubic Spline 1', 
        'Cubic Spline 2', 'Cubic Spline 3'),
	stringsAsFactors = FALSE )

#
getCoefTable(varKey, modList, modNames, 'reiter_stam', 'Reiter \\& Stam (2003)', plotPath, 3)
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