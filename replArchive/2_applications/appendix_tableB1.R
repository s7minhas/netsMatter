#############################
# set a path
require(here)
pth = paste0(here::here(), '/')
rsPth = paste0(pth, '2_applications/application_data/reiter_stam/')

#
source(paste0(pth, 'helpers/clusteredSE.R'))
source(paste0(pth, 'helpers/functions.R'))
source(paste0(pth, 'helpers/stargazerHelpers.R'))
##############################

##############################
# load models
load(paste0(rsPth,'ameFitReiterStam.rda'))

if(!file.exists(paste0(rsPth,'glmFitReiterStam.rda'))){
	source(paste0(pth, '2_applications/reiter_stam_glmRun.R')) }
load( paste0(rsPth,'glmFitReiterStam.rda') )
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
	clean=c('Intercept', 'Pers/Democ Directed Dyad',
		'Democ/Pers Directed Dyad',
        'Personal', 'Military', 'Single', 'Democracy',
        'Contiguous', 'Major Power', 'Ally', 'Higher/Lower Power Ratio',
        'Economically Advanced', 'Years Since Last Dispute', 'Cubic Spline 1',
        'Cubic Spline 2', 'Cubic Spline 3'),
	stringsAsFactors = FALSE )

#
getCoefTable(
  varKey, modList, modNames,
  'appendix_tableB1', 'Reiter \\& Stam (2003)', pth, 3)
##############################
