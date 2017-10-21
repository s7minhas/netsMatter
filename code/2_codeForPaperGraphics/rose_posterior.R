# setup ###########################################
rm(list=ls())

if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	resultsPath = '~/Dropbox/Research/netsMatter/replications/rose2004/'
	plotPath = '~/Research/netsMatter/paper/' }

source('~/Research/netsMatter/code/helpers/functions.R')
source('~/Research/netsMatter/code/helpers/ameHelpers.R')
############################################

# modData ###########################################
load(paste0(resultsPath,'outputData/ameFit_k2_v2_imps_50000_intercept.rda'))
load( paste0(resultsPath,'glmResults_rose.rda') )
############################################

# coef summ ###########################################
# get ame est
ameSumm = t(apply(ameFit$BETA, 2, function(x){ c(
	mu=mean(x),
	quantile(x, probs=c(0.025,0.05,0.95,0.975))) }))
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