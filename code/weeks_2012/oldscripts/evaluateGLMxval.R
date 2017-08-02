## This file to get data about the GLM
## cross-val

dataPath <-'~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'


## warning: this takes almost 30 minute to load!
load(paste0(dataPath,"weeksOutPerf.rda"))

ls()

outFitGLM <- glmOutSamp_wFullSpec

attributes(outFitGLM)

outFitGLM$aucPR
outFitGLM$aucROC


outFitGLM$aucByFold


