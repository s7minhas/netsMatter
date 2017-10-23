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

# # coefSumm ###########################################
# ameSumm = t(apply(ameFit$BETA, 2, function(x){ c(
# 	'Estimate'=mean(x), 'Std. Error'=sd(x), 'z value'=mean(x)/sd(x),
# 	'Pr(>|z|)'=2*(1-pnorm( abs(mean(x)/sd(x)))) )}))
# rownames(ameSumm) = gsub('.dyad','',rownames(ameSumm),fixed=TRUE)
# rownames(ameSumm) = gsub('_s','s',rownames(ameSumm),fixed=TRUE)
# rownames(ameSumm) = gsub('intercept','(Intercept)',rownames(ameSumm),fixed=TRUE)
# lmSumm = baseModelSumm[,]

# modList = list(lmSumm, ameSumm)
# modNames = c('LM', 'AME')

# varKey = data.frame(
# 	dirty=rownames(ameSumm),
# 	clean=c('Intercept', 'Both in GATT/WTO', 'One in GATT/WTO', 'GSP',
# 		'Log Distance',  'Log Product Real GDP', 'Log Product Real GDPpc',
# 		'Regional FTA', 'Currency Union', 'Common language', 'Land Border',
# 		'Number Landlocked' ,'Number Islands', 'Log Product Land Area',
# 		'Common Colonizer','Currently Colonized', 'Ever Colony','Common Country' ),
# 	stringsAsFactors = FALSE )

# #
# getCoefTable(varKey, modList, modNames, 'rose', 'Rose (2004)', plotPath, 3)
# ############################################

# outPerf ###########################################
load(paste0(resultsPath, 'outputData/ameCrossvalResults_k2_v2_imps_50000_intercept_30folds.rda')) # ameOutSamp
load(paste0(resultsPath,'glmCrossValResults_small.rda')) # glmOutSamp

# org
oSamp = rbind(
	cbind(data.frame(glmOutSamp$rmseByFold), model='GLM'),
	cbind(data.frame(ameOutSamp$rmseByFold), model='AME')
	) %>% select(-fold) %>%
	gather(key='stat',value='value',-model)

# summ over
oSampStats = oSamp %>% 
	group_by(model, stat) %>%
	dplyr::summarize(
		mu=mean(value), 
		lo95=quantile(value,probs=.025),
		lo90=quantile(value,probs=.05),
		hi90=quantile(value,probs=.95),				
		hi95=quantile(value,probs=.975)
		)

# lab cleanup
oSampStats$stat[oSampStats$stat=='rmse'] = 'RMSE'
oSampStats$stat[oSampStats$stat=='rmdse'] = 'RMDSE'
oSampStats$stat[oSampStats$stat=='r2'] = '$R^{2}$'
oSampStats$stat = factor(oSampStats$stat, levels=c('RMSE','RMDSE','$R^{2}$'))

# add text
oSampStats$lab = paste0(
	'Avg. = ', round(oSampStats$mu,2), 
	'\n95% Interval = [', round(oSampStats$lo95,2), ' - ', round(oSampStats$hi95,2),']' )
oSampStats$labY = c(.1, 1.8, 3.0, .6, 1.1, 2.0)

# viz
modCols = c(GLM='#d6604d', AME='#4393c3')
facet_labeller = function(string){ TeX(string) }
ggOsamp = ggplot(oSampStats, aes(x=model, y=mu, color=model)) + 
	geom_point() + xlab('') + ylab('') + 
	geom_linerange(aes(ymin=lo90,ymax=hi90),size=.7) +		
	geom_linerange(aes(ymin=lo95,ymax=hi95),size=.3) +	
	geom_text(aes(label=lab, y=labY), size=2.5, color='black') +
	scale_color_manual(values=modCols) + 
	facet_wrap(~stat, scales='free_y',
			labeller=as_labeller(facet_labeller, default = label_parsed)) +
	theme(
		axis.ticks=element_blank(),
		panel.border=element_blank(),
		legend.position='none',
		axis.text.x=element_text(size=10, face='bold'),
		strip.text.x = element_text(size=9, color='white'),
		strip.text.y = element_text(size=9, color='white', angle=0),		
		strip.background = element_rect(fill = "#525252", color='#525252')		
		)
ggsave(ggOsamp, file=paste0(plotPath, 'rose_outSample.pdf'), width=8, height=3)
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