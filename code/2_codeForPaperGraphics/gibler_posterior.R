# setup ###########################################
rm(list=ls())

if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	resultsPath = '~/Dropbox/Research/netsMatter/replications/gibler_2017/outputData/'
	plotPath = '~/Dropbox/Research/netsMatter/replications/0_finalRepFigs/' }

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

# plot of first diffs ###########################################

############################################