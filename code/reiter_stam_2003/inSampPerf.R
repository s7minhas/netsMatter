########
# What does this do?
#######

#
rm(list=ls())


# load libraries
packs = c()

#
source("LoadPkg.R")

#
loadPkg(packs)

# read data
resultsPath = '/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/'
load(paste0(resultsPath, 'amenData.rda'))

# logit model
load( paste0(resultsPath,'reiterStam_baseModel.rda') )
logitModProbs = 1/(1+exp(-predict(mod1)))
logitPred = data.frame(
  actual=mod_dat$sideaa,prob=logitModProbs,
  row=nrow(mod_dat),col=ncol(mod_dat))
logitPred = logitPred[logitPred$row != logitPred$col,]
################################################

################################################
# Amen
load(paste0(resultsPath, 'ameFitSR_2.rda'))
rownames(yList) = colnames(yList) = as.character(1:34)
diag(yList) = NA; actual = melt(yList)
preds = ameFit$'EZ'
rownames(preds) = colnames(preds) = char(1:34)
diag(preds) = NA; amePred = melt(preds)

amePred$prob = 1/(1+exp(-amePred$value))
amePred$actual = actual$value
amePred = na.omit(amePred)
################################################