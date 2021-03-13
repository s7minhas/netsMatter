#############################
# set a path
require(here)
pth = paste0(here::here(), '/replArchive/')
gPth = paste0(pth, '2_applications/application_data/gibler/')

#
source(paste0(pth, 'helpers/clusteredSE.R'))
source(paste0(pth, 'helpers/functions.R'))
source(paste0(pth, 'helpers/ameHelpers.R'))

#
library(mvtnorm)
##############################

##############################
# load models
load(paste0(gPth,'ameFitGibler.rda'))
load( paste0(gPth,'glmFitGibler.rda') )
##############################

##############################
# plot marg effs
# get data and add intercept val
modData = mod$model ; modData$intercept = 1

# create scenario matrix
vars = colnames(ameFit$BETA)
varsMod = gsub('.dyad','',vars,fixed=TRUE) %>% gsub('_s','s',.,fixed=TRUE)
medVals = apply(modData[,varsMod], 2, median) ; names(medVals) = varsMod
replaceVal = function(var, newVal, oVals=medVals){
	oVals[var] = newVal ; return(oVals)  }
getQ = function(x,p,data=modData){ quantile(data[,x], probs=p, na.rm=TRUE) }
scen = cbind(
	ally_1=replaceVal('allied', 1),
	ally_0=replaceVal('allied', 0),
	jdem_1=replaceVal('jointdem', 1),
	jdem_0=replaceVal('jointdem', 0),
	contig_1=replaceVal('contig', 1),
	contig_0=replaceVal('contig', 0),
	riv_1=replaceVal('riv1', 1),
	riv_0=replaceVal('riv1', 0),
	fpar_hi=replaceVal('firstparity', getQ('firstparity',.75)),
	fpar_lo=replaceVal('firstparity', getQ('firstparity',.25)),
	par_hi=replaceVal('parity4', getQ('parity4',.75)),
	par_lo=replaceVal('parity4', getQ('parity4',.25)) )
# split into hi and lo
scen1 = scen[,seq(1,ncol(scen),2)] ; scen0 = scen[,seq(2,ncol(scen),2)]
scens = c( 'Allied', 'Joint Democracy', 'Contiguity',
	'Rivalry', 'Parity at entry year', 'Parity')

# for ame
glmDraws = rmvnorm(1000, coef(mod), vcov(mod)) ; colnames(glmDraws)[1] = 'intercept'
ameDraws = rmvnorm(1000, apply(ameFit$BETA, 2, median), cov(ameFit$BETA))
scenDiffs = rbind(
	getScenDiff(linkType='logit', scen1, scen0, scens, glmDraws, 'GLM', type='densityShade'),
	getScenDiff(linkType='probit', scen1, scen0, scens, ameFit$BETA, 'AME', type='densityShade') )

ggCols = c(GLM='#d6604d', AME='#4393c3')
ggLty = c(GLM='dashed', AME='solid')
scenDiffsSlice = scenDiffs[scenDiffs$scen %in% c('Rivalry'),]
scenGG = ggplot(data=scenDiffsSlice, aes(color=mod, fill=mod)) +
	geom_line(data=scenDiffsSlice, aes(x=x,y=y)) +
	geom_ribbon(data=subset(scenDiffsSlice,q95), aes(x=x,ymax=y,fill=mod),ymin=0,alpha=0.2) +
	geom_ribbon(data=subset(scenDiffsSlice,q90), aes(x=x,ymax=y,fill=mod),ymin=0,alpha=0.6) +
	geom_vline(aes(xintercept=mean, color=mod,linetype=mod),size=1.2) +
	scale_color_manual(values=ggCols) + scale_fill_manual(values=ggCols) +
	scale_linetype_manual(values=ggLty) +
	guides(
		linetype=guide_legend(override.aes = list(size=.5)),
		fill=guide_legend(override.aes = list(fill='transparent'))
		) +
	xlab('Pr(MID=1 | Rivalry=1) - Pr(MID=1 | Rivalry=0)') +
	ylab('Density') +
	facet_wrap(~scen, scales='free', ncol=1) +
	theme(
		legend.position = 'top', legend.title=element_blank(),
		axis.ticks=element_blank(), axis.text.y=element_blank(),
		panel.border=element_blank(),
		strip.text.x = element_text(size = 9, color='white' ),
		strip.background = element_rect(fill = "#525252", color='#525252')
		)
ggsave(scenGG, file=paste0(pth, 'figure9.pdf'), width=7, height=3)
############################################
