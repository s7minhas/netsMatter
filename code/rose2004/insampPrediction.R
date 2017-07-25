########  in sample performance test. this is pretty simple.
rm(list=ls())
source('~/Research/netModels/code/helpers/paths.R')
source(paste0(funcPath, 'functions.R'))
source(paste0(funcPath, 'binPerfHelpers.R'))
loadPkg(c('png','grid'))

# load data
load(paste0(dataPath, 'data.rda'))

################################################
# Logit Mod
load("/Users/howardliu/Dropbox/netsMatter/replications/rose2004/glmResults_rose.rda")
model.nrm = mod1
nrm.data = mod1_data

nrmModProbs = predict(model.nrm)
nrmPred = data.frame(
	actual=nrm.data$ltrade,predicted=nrmModProbs,
	statea = nrm.data$cty1,
	stateb = nrm.data$cty2
)
nrmPred = nrmPred[nrmPred$statea != nrmPred$stateb,]
head(nrmPred)
RMSE_nrm = sqrt(mean((nrmPred$predicted - nrmPred$actual)^2))
RMSE_nrm
################################################

################################################
# Amen - K0
resultsPath = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/outputData/"
dataPath = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/"
load( paste0(dataPath,'amenData_rose.rda') )
actual = melt(yList)
preds = ameFit$'EZ'
amePred_k0 = melt(preds)

amePred_k0$predicted = amePred_k0
amePred_k0$actual = actual$value
amePred_k0 = na.omit(amePred_k0)

predDfs = list(Normal=nrmPred, AME_k0 = amePred_k0)
head(amePred_k0)

RMSE_ame = sqrt(mean((amePred_k0$value - amePred_k0$actual)^2))
RMSE_ame


# Amen - K1
resultsPath = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/outputData/"
load( paste0(resultsPath,'ameFit_k1_re.rda') )
dataPath = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/"
load( paste0(dataPath,'amenData_rose.rda') )
actual = melt(yList)
preds = ameFit$'EZ'
amePred_k1 = melt(preds)

amePred_k1$predicted = amePred_k1
amePred_k1$actual = actual$value
amePred_k1 = na.omit(amePred_k1)

# Organize pred DFs
#predDfs = list(Normal=nrmPred, AME_k1 = amePred_k1)
head(amePred_k1)

RMSE_ame1 = sqrt(mean((amePred_k1$value - amePred_k1$actual)^2))
RMSE_ame1

# Amen - K2
resultsPath = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/outputData/"
load( paste0(resultsPath,'ameFit_k2_v1_rose.rda') )
dataPath = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/"
load( paste0(dataPath,'amenData_rose.rda') )
actual = melt(yList)
preds = ameFit$'EZ'
amePred_k2 = melt(preds)

amePred_k2$predicted = amePred_k2
amePred_k2$actual = actual$value
amePred_k2 = na.omit(amePred_k2)

# Organize pred DFs
#predDfs = list(Normal=nrmPred, AME_k1 = amePred_k1)
head(amePred_k2)

RMSE_ame2 = sqrt(mean((amePred_k2$value - amePred_k2$actual)^2))
RMSE_ame2

# Amen - K3
resultsPath = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/outputData/"
load( paste0(resultsPath,'ameFit_k3_v1.rda') )
dataPath = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/"
load( paste0(dataPath,'amenData_rose.rda') )
actual = melt(yList)
preds = ameFit$'EZ'
amePred_k3 = melt(preds)

amePred_k3$predicted = amePred_k3
amePred_k3$actual = actual$value
amePred_k3 = na.omit(amePred_k3)

# Organize pred DFs
#predDfs = list(Normal=nrmPred, AME_k1 = amePred_k1)
head(amePred_k3)

RMSE_ame3 = sqrt(mean((amePred_k3$value - amePred_k3$actual)^2))
RMSE_ame3

# loadPkg(c('ROCR', 'caTools','RColorBrewer'))
# # Auc, depends ROCR
# getAUC = function(prediction, actual){
#   pred = prediction(prediction, actual)
#   attributes(performance(pred,"auc"))$y.values[[1]]
# }
#
# # Area under Precision-recall curve
# auc_pr <- function(obs, pred) {
#   xx.df <- prediction(pred, obs)
#   perf  <- performance(xx.df, "prec", "rec")
#   xy    <- data.frame(recall=perf@x.values[[1]], precision=perf@y.values[[1]])
#   xy <- subset(xy, !is.nan(xy$precision))
#   res   <- trapz(xy$recall, xy$precision)
#   return(res) }
#
# # get auc summary
# aucSumm = do.call('rbind',
#                   lapply(predDfs, function(x){
#                     cbind( 'AUC'=getAUC(x$prob, x$actual), 'AUC (PR)'=auc_pr(x$actual, x$prob) )
#                   } ) ) ; rownames(aucSumm) = names(predDfs)
# aucSumm = aucSumm[order(aucSumm[,1],decreasing=TRUE),]
# aucSumm = trim(format(round(aucSumm, 2), nsmall=2))
################################################
