########
# What does this do?
#######

#
rm(list=ls())


# load libraries
packs = c('reshape2', 'tidyverse')

#
source("LoadPkg.R")
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

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
  statea = mod_dat$statea,
  stateb = mod_dat$stateb)
logitPred = logitPred[logitPred$statea != logitPred$stateb,]
################################################

################################################
# Amen - K0
load(paste0(resultsPath, 'model_k0_v12.rda'))
actual = melt(yList)
preds = ameFit$'EZ'
amePred_k0 = melt(preds)
amePred_k0$prob = 1/(1+exp(-amePred_k0$value))
amePred_k0$actual = actual$value
amePred_k0 = na.omit(amePred_k0)
# Amen - K1
load(paste0(resultsPath, 'model_k1_v12.rda'))
actual = melt(yList)
preds = ameFit$'EZ'
amePred_k1 = melt(preds)
amePred_k1$prob = 1/(1+exp(-amePred_k1$value))
amePred_k1$actual = actual$value
amePred_k1 = na.omit(amePred_k1)
# Amen - K2
load(paste0(resultsPath, 'model_k2_v12.rda'))
actual = melt(yList)
preds = ameFit$'EZ'
amePred_k2 = melt(preds)
amePred_k2$prob = 1/(1+exp(-amePred_k2$value))
amePred_k2$actual = actual$value
amePred_k2 = na.omit(amePred_k2)
# Amen - K3
load(paste0(resultsPath, 'model_k3_v12.rda'))
actual = melt(yList)
preds = ameFit$'EZ'
amePred_k3 = melt(preds)
amePred_k3$prob = 1/(1+exp(-amePred_k3$value))
amePred_k3$actual = actual$value
amePred_k3 = na.omit(amePred_k3)
################################

# Organize pred DFs
predDfs = list( Logit=logitPred, AME_k0 = amePred_k0, 
                AME_k1 = amePred_k1, AME_k2 = amePred_k2, 
                AME_k3 = amePred_k3)

loadPkg(c('ROCR', 'caTools','RColorBrewer'))
# Auc, depends ROCR
getAUC = function(prediction, actual){
  pred = prediction(prediction, actual) 
  attributes(performance(pred,"auc"))$y.values[[1]]
}

#' Area under Precision-recall curve
auc_pr <- function(obs, pred) {
  xx.df <- prediction(pred, obs)
  perf  <- performance(xx.df, "prec", "rec")
  xy    <- data.frame(recall=perf@x.values[[1]], precision=perf@y.values[[1]])
  xy <- subset(xy, !is.nan(xy$precision))
  res   <- trapz(xy$recall, xy$precision)
  return(res) }

# get auc summary
aucSumm = do.call('rbind', 
                  lapply(predDfs, function(x){ 
                    cbind( 'AUC'=getAUC(x$prob, x$actual), 'AUC (PR)'=auc_pr(x$actual, x$prob) ) 
                  } ) ) ; rownames(aucSumm) = names(predDfs)
aucSumm = aucSumm[order(aucSumm[,1],decreasing=TRUE),]
aucSumm = trim(format(round(aucSumm, 2), nsmall=2))
################################################