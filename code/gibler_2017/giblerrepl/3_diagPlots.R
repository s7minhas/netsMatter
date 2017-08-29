rm(list = ls())

if(Sys.info()['user']=='s7m'){
	dropPath = '~/Dropbox/Research/netsMatter/replications/gibler_2017/'
	funcPath = '~/Research/netsMatter/code/helpers/'
	resultsPath = paste0(dropPath, 'outputData/')	
}

# load data and past results
library(amen)
load(paste0(dropPath, "amenData_gibler2.rda"))
load(paste0(resultsPath, 'mdwameFit_k2.rda'))

#
gofPlot(ameFit$GOF, TRUE)
paramPlot(ameFit$BETA)

source(paste0(funcPath, 'ameHelpers.R'))
addEffPlot(ameFit)