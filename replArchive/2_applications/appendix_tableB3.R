#############################
# set a path
require(here)
pth = paste0(here::here(), '/')
gPth = paste0(pth, '2_applications/application_data/gibler/')

#
source(paste0(pth, 'helpers/clusteredSE.R'))
source(paste0(pth, 'helpers/functions.R'))
source(paste0(pth, 'helpers/stargazerHelpers.R'))
##############################

##############################
# load models

# load ame mods run in parallel
files = list.files(gPth)[grepl('ameFitGibler_', list.files(gPth))]
pthFiles = paste0(gPth, files)

# gather together into new ameFit$BETA object
ameFit=list()
ameFit$BETA = do.call('rbind', lapply(pthFiles, function(pthFile){
	load(pthFile) ; return( ameFit$BETA ) } ) )

# load glm data
load( paste0(gPth,'glmFitGibler.rda') )
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

dat = cbind(y=mod$y, mod$data[,names(coef(mod)[-1])], dyad=mod$data$dyad)
form = paste0('y~',paste(names(coef(mod)[-1]), collapse='+'))
probMod = glm( form, data=dat, family=binomial(link='probit') )
loadPkg('lmtest')
probSumm = coeftest(probMod, vcov = vcovCluster(mod, cluster = dat$dyad))[,]

modList = list(glmSumm, ameSumm)
modNames = c('GLM (Logit)', 'AME')
varKey = data.frame(
	dirty=names(coef(mod)),
	clean=c( '(Intercept)',
		'Allied', 'Joint Democracy',
		'Peace Years', 'Spline 1', 'Spline 2',
		'Spline 3', 'Contiguity', 'Parity',
		'Parity at Entry Year', 'Rivalry' ),
	stringsAsFactors = FALSE )

#
getCoefTable(
	varKey, modList, modNames,
	'appendix_tableB3', 'Gibler (2017)', pth, 3)
##############################
