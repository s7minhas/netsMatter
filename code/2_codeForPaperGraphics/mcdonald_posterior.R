# setup ###########################################
rm(list=ls())

if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	resultsPath = '~/Dropbox/Research/netsMatter/replications/McDonald_2004/data/'
	inputPath= '~/Dropbox/Research/netsMatter/replications/McDonald_2004/data/'
	plotPath = '~/Research/netsMatter/paper/' }

source('~/Research/netsMatter/code/helpers/functions.R')
source('~/Research/netsMatter/code/helpers/ameHelpers.R')
source('~/Research/netsMatter/code/helpers/binPerfHelpers.R')
source('~/Research/netsMatter/code/helpers/stargazerHelpers.R')
############################################

# modData ###########################################
load(paste0(resultsPath, 'mcdonald_glmfit.rda'))
load( paste0(resultsPath,'ameFitIntercepts_k2.rda') )
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
	clean=c( '(Intercept)', 
		"Spline0","Spline1","Spline2","Spline3",
		"Shared Alliance","Contiguous","Log Capabilities Ratio",
		"Trade Dependence","Preconflict GDP Change","Lowest Dyadic Polity Score",
		"Capabilities","Logged GDP","Logged Cap. Distance","Major Power In Dyad",
		"Higest Barrier To Trade"),
	stringsAsFactors = FALSE )

#
getCoefTable(varKey, modList, modNames, 'mcdonald', 'McDonald (2004)', plotPath, 3)
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