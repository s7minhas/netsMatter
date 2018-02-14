# setup ###########################################
rm(list=ls())

if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	resultsPath = '~/Dropbox/Research/netsMatter/replications/Reiter_Stam_2003/output/'
	inputPath= '~/Dropbox/Research/netsMatter/replications/Reiter_Stam_2003/input/'
	plotPath = '~/Research/netsMatter/paper/'
  source('~/Research/netsMatter/code/helpers/functions.R')
  source('~/Research/netsMatter/code/helpers/ameHelpers.R')
  source('~/Research/netsMatter/code/helpers/binPerfHelpers.R')
  source('~/Research/netsMatter/code/helpers/stargazerHelpers.R') }

if(Sys.info()['user'] == 'juanftellez'){
  resultsPath = '/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/'
  plotPath = '~/OneDrive/netsMatter/paper/'
  source('~/OneDrive/netsMatter/code/helpers/ameHelpers.R')
  source('helperEx.R')
}
############################################

# modData ###########################################
load( paste0(resultsPath,'reiter_stam_glmfit.rda') )
load( paste0(resultsPath,'model_k2_v12.rda') )
############################################

# # coefSumm ###########################################
# ameSumm = t(apply(ameFit$BETA, 2, function(x){ c(
# 	'Estimate'=mean(x), 'Std. Error'=sd(x), 'z value'=mean(x)/sd(x),
# 	'Pr(>|z|)'=2*(1-pnorm( abs(mean(x)/sd(x)))) )}))
# rownames(ameSumm) = gsub('.row','',rownames(ameSumm),fixed=TRUE)
# rownames(ameSumm) = gsub('.col','',rownames(ameSumm),fixed=TRUE)
# rownames(ameSumm) = gsub('.dyad','',rownames(ameSumm),fixed=TRUE)
# rownames(ameSumm) = gsub('_s','s',rownames(ameSumm),fixed=TRUE)
# rownames(ameSumm) = gsub('intercept','(Intercept)',rownames(ameSumm),fixed=TRUE)
# glmSumm = modSumm[,]
# probSumm = proSumm[,]

# modList = list(glmSumm, probSumm, ameSumm)
# modNames = c('GLM (Logit)','GLM (Probit)', 'AME')

# varKey = data.frame(
# 	dirty=names(coef(mod)),
# 	clean=c('Intercept', 'Pers/Democ Directed Dyad',
# 		'Democ/Pers Directed Dyad',
#         'Personal', 'Military', 'Single', 'Democracy', 
#         'Contiguous', 'Major Power', 'Ally', 'Higher/Lower Power Ratio', 
#         'Economically Advanced', 'Years Since Last Dispute', 'Cubic Spline 1', 
#         'Cubic Spline 2', 'Cubic Spline 3'),
# 	stringsAsFactors = FALSE )

# #
# getCoefTable(varKey, modList, modNames, 'reiter_stam', 'Reiter \\& Stam (2003)', plotPath, 3)
# ############################################

# addEffdata ###########################################
# sender effects
effdat = getAddEffData(fit = ameFit) ##This function is in helperEx.R
effdat = effdat[which(
  effdat$addEff>=quantile(effdat$addEff,.9) |
  effdat$addEff<=quantile(effdat$addEff,.1)
  ),]
effdat$actor = countrycode::countrycode(effdat$actor, 'cown', 'country.name')
effdat = na.omit(effdat)
effdat$actor = factor(effdat$actor,  levels=effdat[order(effdat$addEff),'actor'])

## subset of countries
addEffPlot(fit = effdat, addEffData = effdat, row = T) + coord_flip()

# receiver effects
effdat = getAddEffData(fit = ameFit, row=FALSE) ##This function is in helperEx.R
effdat = effdat[which(
  effdat$addEff>=quantile(effdat$addEff,.9) |
  effdat$addEff<=quantile(effdat$addEff,.1)
  ),]
effdat$actor = countrycode::countrycode(effdat$actor, 'cown', 'country.name')
effdat = na.omit(effdat)
effdat$actor = factor(effdat$actor,  levels=effdat[order(effdat$addEff),'actor'])

## subset of countries
addEffPlot(fit = effdat, addEffData = effdat, row = FALSE) + coord_flip()
############################################

# # outPerf ###########################################
# load(paste0(resultsPath, 'ameCrossValResults_k2.rda')) # ameOutSamp_k2
# ameOutSamp=ameOutSamp_k2 ; rm(ameOutSamp_k2)
# load(paste0(resultsPath,'glmCrossValResults.rda')) # glmOutSamp
# glmOutSamp=glmOutSamp_wFullSpec ; rm(glmOutSamp_wFullSpec)
# toKeep = which(!is.na(glmOutSamp$outPerf$pred))

# # org
# predDfs = list(
# 	GLM = data.frame(actual=glmOutSamp$outPerf$actual[toKeep], pred=glmOutSamp$outPerf$pred[toKeep], model='GLM'),
# 	AME = data.frame(actual=ameOutSamp$outPerf$actual, pred=ameOutSamp$outPerf$pred, model='AME') )

# # run
# ggPerfCurves(predDfs, 'reiter_stam')
# ############################################

# marg eff plots ###########################################
# create scenario matrix
modData = mod$model ; modData$intercept = 1

vars = colnames(ameFit$BETA)
varsMod = gsub('.dyad','',vars,fixed=TRUE) %>% gsub('_s','s',.,fixed=TRUE)
medVals = apply(modData[,varsMod], 2, median) ; names(medVals) = varsMod
replaceVal = function(var, newVal, oVals=medVals){
  oVals[var] = newVal ; return(oVals)  }
getQ = function(x,p,data=modData){ quantile(data[,x], probs=p, na.rm=TRUE) }
scen = cbind(
  pdemtar_1=replaceVal('pdemdtar', 1), 
  pdemtar_0=replaceVal('pdemdtar', 0),
  pdemin_1=replaceVal('pdemdin', 1), 
  pdemin_0=replaceVal('pdemdin', 0))
# split into hi and lo
scen1 = scen[,seq(1,ncol(scen),2)] ; scen0 = scen[,seq(2,ncol(scen),2)]
scens = c( 'Pers/Democ Directed Dyad', 'Democ/Pers Directed Dyad')

# # for ame
# scenDiffs =
#   getScenDiff(linkType='logit', scen1, scen0, scens, ameFit$BETA, 'AME', type='densityShade')
# scenDiffs$title = 'Regime Directed Dyad'
# scenDiffs$scen = gsub('.', ' ',scenDiffs$scen, fixed = TRUE)
# for ame
glmDraws = rmvnorm(1000, coef(mod), vcov(mod)) ; colnames(glmDraws)[1] = 'intercept'
ameDraws = rmvnorm(1000, apply(ameFit$BETA, 2, median), cov(ameFit$BETA))
scenDiffs = rbind(
  getScenDiff(linkType='logit', scen1, scen0, scens, 
    glmDraws[,rownames(scen1)], 'GLM', type='densityShade'),
  getScenDiff(linkType='probit', scen1, scen0, scens, 
    ameFit$BETA, 'AME', type='densityShade') )


ggCols = c(GLM='#d6604d', AME='#4393c3')
ggLty = c(GLM='dashed', AME='solid')
# scenDiffsSlice = scenDiffs[scenDiffs$scen %in% c('Rivalry'),]
scenDiffsSlice = scenDiffs
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
  # xlab('Pr(MID=1 | Rivalry=1) - Pr(MID=1 | Rivalry=0)') +
  ylab('Density') +
  facet_wrap(mod~scen, scales='free', ncol=1) +
  theme(
    legend.position = 'top', legend.title=element_blank(),
    axis.ticks=element_blank(), axis.text.y=element_blank(),
    panel.border=element_blank(),
    strip.text.x = element_text(size = 9, color='white' ),
    strip.background = element_rect(fill = "#525252", color='#525252')    
    )
scenGG

ggCols = c(`Democ Pers Directed Dyad`='#d6604d', `Pers Democ Directed Dyad`='#4393c3')
ggLty = c(`Democ Pers Directed Dyad`='dashed', `Pers Democ Directed Dyad`='solid')

# scenDiffsSlice = scenDiffs[scenDiffs$scen %in% c('Democ.Pers.Directed.Dyad'),]
scenGG = ggplot(data=scenDiffs, aes(color=scen, fill=scen)) +
  geom_line(data=scenDiffs, aes(x=x,y=y)) +
  geom_ribbon(data=subset(scenDiffs,q95), aes(x=x,ymax=y,fill=scen),ymin=0,alpha=0.2) +
  geom_ribbon(data=subset(scenDiffs,q90), aes(x=x,ymax=y,fill=scen),ymin=0,alpha=0.6) +
  geom_vline(aes(xintercept=mean, color=scen,linetype=scen),size=1.2) +	
  scale_color_manual(values=ggCols) + scale_fill_manual(values=ggCols) +
  scale_linetype_manual(values=ggLty) +
  guides(
    linetype=guide_legend(override.aes = list(size=.5)),
    fill=guide_legend(override.aes = list(fill='transparent'))
  ) +
  xlab('Pr(MID=1 | Regime Directed Dyad=1) - Pr(MID=1 | Regime Directed Dyad=0)') +
  ylab('Density') +
  facet_wrap(~title, scales='free', ncol=1) +
  theme_bw() + 
  theme(
    legend.position = 'top', legend.title=element_blank(),
    axis.ticks=element_blank(), axis.text.y=element_blank(),
    panel.border=element_blank(),
    strip.text.x = element_text(size = 9, color='white' ),
    strip.background = element_rect(fill = "#525252", color='#525252')		
  )
ggsave(scenGG, file=paste0(plotPath, 'reiterstam_margeff.pdf'), width=7, height=3)
############################################