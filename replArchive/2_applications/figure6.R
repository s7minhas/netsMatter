#############################
# set a path
require(here)
pth = paste0(here::here(), '/replArchive/')
rsPth = paste0(pth, '2_applications/application_data/reiter_stam/')
wPth = paste0(pth, '2_applications/application_data/weeks/')
gPth = paste0(pth, '2_applications/application_data/gibler/')

# load in helper functions for ameOutSamp
source(paste0(pth, 'helpers/functions.R'))
loadPkg(c('ROCR', 'RColorBrewer', 'caTools'))
source(paste0(pth, 'helpers/binPerfHelpers.r'))
source(paste0(pth, 'helpers/ameOutSamp.R'))
##############################

##############################
# load data
load(paste0(rsPth, 'ameOutSampReiterStam.rda'))
# load(paste0(wPth, 'ameOutSampWeeks.rda'))
# ameOutSamp_k2
##############################

##############################
# outPerf ###########################################
load(paste0(resultsPath, 'ameCrossValResults_k2.rda')) # ameOutSamp_k2
ameOutSamp=ameOutSamp_k2 ; rm(ameOutSamp_k2)
load(paste0(resultsPath,'glmCrossValResults.rda')) # glmOutSamp
glmOutSamp=glmOutSamp_wFullSpec ; rm(glmOutSamp_wFullSpec)
toKeep = which(!is.na(glmOutSamp$outPerf$pred))

# org
predDfs = list(
  GLM = data.frame(actual=glmOutSamp$outPerf$actual[toKeep], pred=glmOutSamp$outPerf$pred[toKeep], model='GLM'),
  AME = data.frame(actual=ameOutSamp$outPerf$actual, pred=ameOutSamp$outPerf$pred, model='AME') )

# run
ggPerfCurves(predDfs, 'reiter_stam')
############################################
##############################
