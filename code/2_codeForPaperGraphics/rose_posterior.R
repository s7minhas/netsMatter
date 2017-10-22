# setup ###########################################
rm(list=ls())

if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	resultsPath = '~/Dropbox/Research/netsMatter/replications/rose2004/'
	plotPath = '~/Research/netsMatter/paper/' }

source('~/Research/netsMatter/code/helpers/functions.R')
source('~/Research/netsMatter/code/helpers/ameHelpers.R')
source('~/Research/netsMatter/code/helpers/stargazerHelpers.R')
############################################

# modData ###########################################
load(paste0(resultsPath,'outputData/ameFit_k2_v2_imps_50000_intercept.rda'))
load( paste0(resultsPath,'glmResults_rose.rda') )
############################################

# coefSumm ###########################################
ameSumm = t(apply(ameFit$BETA, 2, function(x){ c(
	'Estimate'=mean(x), 'Std. Error'=sd(x), 'z value'=mean(x)/sd(x),
	'Pr(>|z|)'=2*(1-pnorm( abs(mean(x)/sd(x)))) )}))
rownames(ameSumm) = gsub('.dyad','',rownames(ameSumm),fixed=TRUE)
rownames(ameSumm) = gsub('_s','s',rownames(ameSumm),fixed=TRUE)
rownames(ameSumm) = gsub('intercept','(Intercept)',rownames(ameSumm),fixed=TRUE)
lmSumm = baseModelSumm[,]

modList = list(lmSumm, ameSumm)
modNames = c('LM', 'AME')

varKey = data.frame(
	dirty=rownames(ameSumm),
	clean=c('Intercept', 'Both in GATT/WTO', 'One in GATT/WTO', 'GSP',
		'Log Distance',  'Log Product Real GDP', 'Log Product Real GDPpc',
		'Regional FTA', 'Currency Union', 'Common language', 'Land Border',
		'Number Landlocked' ,'Number Islands', 'Log Product Land Area',
		'Common Colonizer','Currently Colonized', 'Ever Colony','Common Country' ),
	stringsAsFactors = FALSE )

#
getCoefTable(varKey, modList, modNames, 'rose', 'Rose (2004)', plotPath, 3)
############################################

# outPerf ###########################################
load(paste0(resultsPath, 'outputData/ameCrossvalResults_k2_v2_imps_50000_intercept_30folds.rda')) # ameOutSamp
load(paste0(resultsPath,'glmCrossValResults_small.rda')) # glmOutSamp

# org
predDfs = list(
	LM = data.frame(actual=glmOutSamp$outPerf$actual, pred=glmOutSamp$outPerf$pred, model='LM'),
	AME = data.frame(actual=ameOutSamp$outPerf$actual, pred=ameOutSamp$outPerf$pred, model='AME') )

# run
glmOutSamp$rmseByFold
ameOutSamp$rmseByFold
############################################

# nodal effects ###########################################
# get abdata and clean actor names
abData = getAddEffData(ameFit)
ifs = read.csv(paste0(resultsPath, 'ifs_countrycode.csv'))
abData$actor = char(ifs$countryname[match(abData$actor, ifs$X.code)])
abData$actor[abData$actor=="LAO PEOPLE'S DEM.REP"] = 'LAOS'

# subset to top 10+ 10- 
toKeep = c(
	abData$actor[order(abData$addEff,decreasing=TRUE)][1:10],
	abData$actor[order(abData$addEff)][1:10] )
abData = abData[abData$actor %in% toKeep,]
abData$actor = str_to_title(abData$actor)
abData$actor = factor(abData$actor, levels=abData$actor[order(abData$addEff)])

# plot
aPlot = addEffPlot(fit=ameFit, addEffData=abData) +
	coord_flip() +
	theme( axis.text.x=element_text(angle=0, size=8) )
ggsave(aPlot, file=paste0(plotPath, 'rose_aeff_top10.pdf'), width=7, height=5)
############################################

# plot srm var ###########################################
plotVC(ameFit$VC, paste0(plotPath, 'rose_srmvc.pdf'), w=7, h=4)
############################################