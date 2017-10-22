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

# calc diffs
getDiff = function(
	linkType='logit', # logit or probit
	scenHi, scenLo, scenNames, 
	beta, modName, type='summStats' # summStats or density
	){
	if(linkType=='logit'){
		predHi = 1/(1+exp(-t(t(scenHi) %*% t(beta))))
		predLo = 1/(1+exp(-t(t(scenLo) %*% t(beta)))) }
	if(linkType=='probit'){
		predHi = pnorm(t(t(scenHi) %*% t(beta)))
		predLo = pnorm(t(t(scenLo) %*% t(beta))) }
	predDiff = predHi-predLo
	colnames(predDiff) = scenNames

	if(type=='summStats'){
		summPred = matrix(NA,nrow=5, ncol=ncol(predDiff),
			dimnames=list(
				c('med','hi95','hi90','lo95','lo90'), colnames(predDiff) ))
		for(s in colnames(summPred)){
			summPred['med',s]=median(predDiff[,s])
			summPred['hi95',s]=quantile(predDiff[,s],.975)
			summPred['hi90',s]=quantile(predDiff[,s],.95)
			summPred['lo95',s]=quantile(predDiff[,s],.025)
			summPred['lo90',s]=quantile(predDiff[,s],.05) }
	
		# org and spit
		summPred = t(summPred) %>% data.frame() %>%
			mutate(
				mod=modName,
				scen=colnames(summPred) )
		}

	if(type=='density'){
		summPred = predDiff %>% data.frame() %>%
			gather(key='scen',value='value') %>%
			mutate(
				mod=modName,
				scenClean=gsub('.',' ', scen, fixed=TRUE)
				)
	}
	return(summPred) }

# for ame
glmDraws = rmvnorm(1000, coef(mod), vcov(mod)) ; colnames(glmDraws)[1] = 'intercept'
ameDraws = rmvnorm(1000, apply(ameFit$BETA, 2, median), cov(ameFit$BETA))
scenDiffs = rbind(
	getDiff(linkType='logit', scen1, scen0, scens, glmDraws, 'GLM', type='density'),
	# getDiff(scen1, scen0, scens, ameDraws, 'AME', type='density') 
	getDiff(linkType='probit', scen1, scen0, scens, ameFit$BETA, 'AME', type='density') )

scenDiffs = scenDiffs[scenDiffs$scen %in% c('Rivalry'),]
scenGG=ggplot(scenDiffs, aes(x=value, fill=mod)) +
	# geom_point(aes(y=med), size=2.5) +
	# geom_linerange(aes(ymin=lo95,ymax=hi95), linetype=1, size=.5) + 
	# geom_linerange(aes(ymin=lo90,ymax=hi90), linetype=1, size=1.5) + 
	geom_density() +
	# coord_flip() + 
	facet_wrap(~scen, ncol=1, scales='free_x')
scenGG
ggsave(scenGG, file=paste0(plotPath, 'gibler_margeff.pdf'), width=7, height=4)

# org and plot
# ggplot(scenDiffs, aes(x=mod)) +
# 	geom_point(aes(y=med), size=2.5) +
# 	geom_linerange(aes(ymin=lo95,ymax=hi95), linetype=1, size=.5) + 
# 	geom_linerange(aes(ymin=lo90,ymax=hi90), linetype=1, size=1.5) + 
# 	coord_flip() + 
# 	facet_wrap(~scen, ncol=1, scales='free_x')
############################################