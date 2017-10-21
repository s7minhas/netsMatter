# setup ###########################################
rm(list=ls())

if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	resultsPath = '~/Dropbox/Research/netsMatter/replications/gibler_2017/outputData/'
	plotPath = '~/Research/netsMatter/paper/' }

source('~/Research/netsMatter/code/helpers/functions.R')
source('~/Research/netsMatter/code/helpers/ameHelpers.R')
############################################

# modData ###########################################
load(paste0(resultsPath,'mdwameFit_k2_v4.rda'))
load( paste0(resultsPath,'glmFit.rda') )
############################################

# coefSumm ###########################################
ameSumm = t(apply(ameFit$BETA, 2, function(x){ c(
	mu=mean(x),
	quantile(x, probs=c(0.025,0.05,0.95,0.975))) }))
############################################

# # plot srm var ###########################################
# # for undirected just do dist of nodal var
# vaViz = ggplot(data.frame(ameFit$VC), aes(x=va)) +
# 	geom_density() +
# 	geom_vline(aes(xintercept=0), linetype=2, color = "black") +
# 	xlab(TeX('Within-Sender Variance ($\\\\sigma_{a]^{2}$)')) +
# 	ylab('Density') +
# 	theme(
# 		axis.ticks=element_blank(), 
# 		panel.border=element_blank()
# 		)
# ggsave(vaViz, file=paste0(plotPath, 'gibler_srmvc.pdf'), width=7, height=4)
# ############################################

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
getDiff = function(scenHi, scenLo, scenNames, 
	beta, modName, type='summStats' # summStats or density
	){
	predHi = 1/(1+exp(-t(t(scenHi) %*% t(beta))))
	predLo = 1/(1+exp(-t(t(scenLo) %*% t(beta))))
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
	getDiff(scen1, scen0, scens, glmDraws, 'GLM', type='density'),
	# getDiff(scen1, scen0, scens, ameDraws, 'AME', type='density') 
	getDiff(scen1, scen0, scens, ameFit$BETA, 'AME', type='density') )

scenDiffs = scenDiffs[scenDiffs$scen %in% c('Rivalry'),]
scenGG=ggplot(scenDiffs, aes(x=value, fill=mod)) +
	# geom_point(aes(y=med), size=2.5) +
	# geom_linerange(aes(ymin=lo95,ymax=hi95), linetype=1, size=.5) + 
	# geom_linerange(aes(ymin=lo90,ymax=hi90), linetype=1, size=1.5) + 
	geom_density() +
	# coord_flip() + 
	facet_wrap(~scen, ncol=1, scales='free_x')

ggsave(scenGG, file=paste0(plotPath, 'gibler_margeff.pdf'), width=7, height=4)

# org and plot
# ggplot(scenDiffs, aes(x=mod)) +
# 	geom_point(aes(y=med), size=2.5) +
# 	geom_linerange(aes(ymin=lo95,ymax=hi95), linetype=1, size=.5) + 
# 	geom_linerange(aes(ymin=lo90,ymax=hi90), linetype=1, size=1.5) + 
# 	coord_flip() + 
# 	facet_wrap(~scen, ncol=1, scales='free_x')
############################################