#############################
# set a path
require(here)
pth = paste0(here::here(), '/')
wPth = paste0(pth, '2_applications/application_data/weeks/')

#
source(paste0(pth, 'helpers/clusteredSE.R'))
source(paste0(pth, 'helpers/functions.R'))
source(paste0(pth, 'helpers/stargazerHelpers.R'))
##############################

##############################
# load models
load(paste0(wPth,'ameFitWeeks.rda'))
load( paste0(wPth,'glmFitWeeks.rda') )
##############################

##############################
ameSumm = t(apply(ameFit$BETA, 2, function(x){ c(
	'Estimate'=mean(x), 'Std. Error'=sd(x), 'z value'=mean(x)/sd(x),
	'Pr(>|z|)'=2*(1-pnorm( abs(mean(x)/sd(x)))) )}))
rownames(ameSumm) = gsub('.row','',rownames(ameSumm),fixed=TRUE)
rownames(ameSumm) = gsub('.col','',rownames(ameSumm),fixed=TRUE)
rownames(ameSumm) = gsub('.dyad','',rownames(ameSumm),fixed=TRUE)
rownames(ameSumm) = gsub('_s','s',rownames(ameSumm),fixed=TRUE)
rownames(ameSumm) = gsub('intercept','(Intercept)',rownames(ameSumm),fixed=TRUE)
glmSumm = modSumm[,]

modList = list(glmSumm, ameSumm)
modNames = c('GLM (Logit)', 'AME')

varKey = data.frame(
	dirty=names(coef(mod)),
	clean=c( '(Intercept)',
		"Machine", "Junta", "Boss", "Strongman",
		"Other Type","New/Unstable Regime", "Democracy Target",
		"Military Capabilities Initiator",
		"Military Capabilities Target ",
		"Initator Share of Capabilities ",## remember this has no AMEN equivalent
		"Low Trade Dependence ",
		"Both Major Powers", "Minor/Major",
		"Major/Minor", "Contiguous", "Log Dist. Between Capitals",
		"Alliance Similarity Dyad ",
		"Alliance Similarity With System Leader Initiator",
		"Alliance Similarity Leader Target",
		"Time Since Last Conflict", "Spline1", "Spline2", "Spline3"),
	stringsAsFactors = FALSE )
varKey = varKey[-11,]

#
getCoefTable(
  varKey, modList, modNames,
  'appendix_tableB2', 'Weeks (2012))', pth, 3)
##############################
