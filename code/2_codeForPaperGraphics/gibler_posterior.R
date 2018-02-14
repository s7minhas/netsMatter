# setup ###########################################
rm(list=ls())

if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	resultsPath = '~/Dropbox/Research/netsMatter/replications/gibler_2017/outputData/'
	plotPath = '~/Research/netsMatter/paper/' }

source('~/Research/netsMatter/code/helpers/functions.R')
source('~/Research/netsMatter/code/helpers/ameHelpers.R')
source('~/Research/netsMatter/code/helpers/binPerfHelpers.R')
source('~/Research/netsMatter/code/helpers/stargazerHelpers.R')
source('~/Research/netsMatter/code/helpers/clusteredSE.R')
############################################

# modData ###########################################
load(paste0(resultsPath,'mdwameFit_k2_v4.rda'))
load( paste0(resultsPath,'glmFit.rda') )
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

dat = cbind(y=mod$y, mod$data[,names(coef(mod)[-1])], dyad=mod$data$dyad)
form = paste0('y~',paste(names(coef(mod)[-1]), collapse='+'))
probMod = glm( form, data=dat, family=binomial(link='probit') )
loadPkg('lmtest')
probSumm = coeftest(probMod, vcov = vcovCluster(mod, cluster = dat$dyad))[,]

modList = list(glmSumm, probSumm, ameSumm)
modNames = c('GLM (Logit)','GLM (Probit)', 'AME')
varKey = data.frame(
	dirty=names(coef(mod)),
	clean=c( '(Intercept)', 
		'Allied', 'Joint Democracy',
		'Peace Years', 'Spline 1', 'Spline 2', 
		'Spline 3', 'Contiguity', 'Parity',
		'Parity at Entry Year', 'Rivalry' ),
	stringsAsFactors = FALSE )

#
getCoefTable(varKey, modList, modNames, 'gibler', 'Gibler (2017)', plotPath, 3)
############################################

# outPerf ###########################################
load(paste0(resultsPath, 'ameCrossVal_k2.rda')) # ameOutSamp_k2
ameOutSamp=ameOutSamp_k2 ; rm(ameOutSamp_k2)
load(paste0(resultsPath,'glmCrossVal.rda')) # glmOutSamp_wFullSpec
glmOutSamp=glmOutSamp_wFullSpec ; rm(glmOutSamp_wFullSpec)

# org
predDfs = list(
	GLM = data.frame(actual=glmOutSamp$outPerf$actual, pred=glmOutSamp$outPerf$pred, model='GLM'),
	AME = data.frame(actual=ameOutSamp$outPerf$actual, pred=ameOutSamp$outPerf$pred, model='AME') )

# run
ggPerfCurves(predDfs, 'gibler')
############################################

# plot srm var ###########################################
# for undirected just do dist of nodal var
vaViz = ggplot(data.frame(ameFit$VC), aes(x=va)) +
	geom_density() +
	geom_vline(aes(xintercept=0), linetype=2, color = "black") +
	xlab(TeX('Within-Sender Variance ($\\\\sigma_{a]^{2}$)')) +
	ylab('Density') +
	theme(
		axis.ticks=element_blank(), 
		panel.border=element_blank()
		)
ggsave(vaViz, file=paste0(plotPath, 'gibler_srmvc.pdf'), width=7, height=4)
############################################

# plot of marg effs ###########################################
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
ggsave(scenGG, file=paste0(plotPath, 'gibler_margeff.pdf'), width=7, height=3)
############################################