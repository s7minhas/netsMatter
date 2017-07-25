################ first, make a table or coefficient plot showing the differences between the GLM and ame estimates?
################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
################

################
# load data

pathResults = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/outputData/"

if(Sys.info()['user']=='howardliu'){
  load('/Users/howardliu/Dropbox/netsMatter/replications/rose2004/amenData_rose.rda' )
  resultsPath = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/"
}

load(paste0(pathResults, 'ameFit_k0_re.rda'))  # load AME mod results
load(paste0(pathResults, 'ameFit_k1_v1.rda'))
load(paste0(pathResults, 'ameFit_k2_v1_rose.rda'))
load(paste0(pathResults, 'ameFit_k3_v1.rda'))
load(paste0(pathResults, 'glmResults_rose.rda')) # load GLM mod results
yArr = listToArray(actors=sort(unique(unlist(lapply(yList,rownames)))),
	Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
################

################
# gather beta estimates from ame fullSpec
fitFullSpec = ameFit
ameBETA = cbind(fitFullSpec$BETA, rho=fitFullSpec$VC[,'rho'])
ameBETA = t(apply(ameBETA, 2, summStats))
colnames(ameBETA) = c('mean','lo95','hi95','lo90','hi90')
ameBETA = data.frame(ameBETA, stringsAsFactors = FALSE)
ameBETA$var = rownames(ameBETA) ; rownames(ameBETA) = NULL
ameBETA$mod = 'AME'

# drop extras and unnecessary params
ameBETA = ameBETA[-which(ameBETA$var %in% c('govActor.dyad','rho')),] # 'rho dropped
#ameBETA$var[ameBETA$var=='postBoko.dyad'] = 'postBoko'
#ameBETA$var[ameBETA$var=='intercept'] = '(Intercept)'

# add in glm estimates
# glmBETA = data.frame(mean=coef(gfitFullSpec),sd=sqrt(diag(vcov(gfitFullSpec))))

glmBETA = data.frame(mean= baseModelSumm[,c("Estimate")],sd= baseModelSumm[,c("Std. Error")])
glmBETA$lo95 = glmBETA$mean - qnorm(.975)*glmBETA$sd
glmBETA$hi95 = glmBETA$mean + qnorm(.975)*glmBETA$sd
glmBETA$lo90 = glmBETA$mean - qnorm(.95)*glmBETA$sd
glmBETA$hi90 = glmBETA$mean + qnorm(.95)*glmBETA$sd
glmBETA$var = rownames(glmBETA) ; rownames(glmBETA) = NULL
glmBETA$mod = 'GLM'
# drop extras and unnecessary params
glmBETA = glmBETA[-which( glmBETA$var %in% c('(Intercept)')),]
glmBETA = glmBETA[1:17,] ## delete fixed effect
glmBETA = glmBETA[,-which( names(glmBETA) %in% c('sd'))]
# combine and cleanup
summ = rbind(ameBETA, glmBETA)
# dirtyVars=c('(Intercept)','postBoko',paste0('riotsAgainst.',c('row','col')),paste0('vioCivEvents.',c('row','col')))
# cleanVars=c(
# 	'(Intercept)', 'Post-Boko\nHaram Period',
# 	'Number of\nRiots/Protests\nAgainst Sender',
# 	'Number of\nRiots/Protests\nAgainst Receiver',
# 	'Number of\nViolent Events Against\nCivilians Committed by Sender',
# 	'Number of\nViolent Events Against\nCivilians Committed by Receiver'
# 	)
#summ$varClean=NA; for(i in 1:length(dirtyVars)){summ$varClean[summ$var==dirtyVars[i]]=cleanVars[i]}
#summ$varClean = factor(summ$varClean, levels=cleanVars)

summ$var = factor(summ$var)
# add sig info
summ$sig = NA
summ$sig[summ$lo90 > 0 & summ$lo95 < 0] = "Positive at 90"
summ$sig[summ$lo95 > 0] = "Positive"
summ$sig[summ$hi90 < 0 & summ$hi95 > 0] = "Negative at 90"
summ$sig[summ$hi95 < 0] = "Negative"
summ$sig[summ$lo90 < 0 & summ$hi90 > 0] = "Insignificant"
coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255),
	"Negative"= rgb(222, 45, 38, maxColorValue=255),
	"Positive at 90"=rgb(158, 202, 225, maxColorValue=255),
	"Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
	"Insignificant" = rgb(150, 150, 150, maxColorValue=255))
################

################
# plot
posDodge = .75
ggCoef=ggplot(summ, aes(x=var, y=mean, color=sig, group=mod)) +
	geom_hline(aes(yintercept=0), linetype=2, color = "black") +
	geom_point(aes(shape=mod), size=2.5, position=position_dodge(width = posDodge)) +
	#geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1, position=position_dodge(width = posDodge)) +
	geom_errorbar(aes(ymin=lo95,ymax=hi95),linetype = 1,width = 0.1, position=position_dodge(width = posDodge)) +
	scale_colour_manual(values = coefp_colors, guide=FALSE) +
	#facet_wrap(~var, ncol=2, scales='free') + xlab('') + ylab('') +
	 coord_flip() + xlab('') + ylab('') #+
	# theme(
	# 	legend.position='top',
	# 	legend.text=element_text(size=10),
	# 	legend.key = element_blank(),
	# 	legend.title=element_blank(),
	# 	panel.border=element_blank(),
	# 	axis.ticks=element_blank(),
	# 	axis.text.x=element_blank(),
	# 	axis.text.y=element_text(family="Source Sans Pro Light"),
	# 	strip.text=element_text(family="Source Sans Pro Light")
	# )
ggCoef

pathGraphics = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/graphics/"
latDim = 0
ggsave(ggCoef, file=paste0(pathGraphics,'betaEst_k',latDim,'.pdf'), width=6, height=10, device=cairo_pdf)
################
