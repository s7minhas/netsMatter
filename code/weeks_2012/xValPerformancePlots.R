
### This is the third of the three scripts
## to generate the Weeks xvalidation

## needs to load model fit (produced by weeks fit crossval)
path <-'~/Dropbox/netsMatter/replications/Weeks2012/replication/output/' 

################

## Load helpers for the out sample validation:

library('ROCR')
library('RColorBrewer')
library('caTools')
source('binPerf.R')

ameOutSamp = function(yList=yList,
    xDyadL=xDyadL,
    xRowL=xRowL,
    xColL=xColL,
    startVals=startVals,
    seed=seed,
    folds=folds,
    R=R,
    model=model,
    burn=burn,
    nscan=nscan,
    odens=odens,
    intercept=intercept,
    rvar=rvar,
    cvar=cvar,
    symmetric=symmetric){
    
    set.seed(6889)
    
    yListFolds = lapply(yList, function(y){
        yFold=matrix(sample(1:folds, length(y), replace=TRUE),
            nrow=nrow(y),ncol=ncol(y), dimnames=dimnames(y)) # Na --> affects folds, so only 4-5 folds
        diag(yFold) = NA
        return(yFold) }) # num of folds
    ## Then:
    print("Getting predictions")

  # get preds (predictions)
outPerf = do.call('rbind', lapply(1:folds, function(f){
    fitFoldPred = fitCrossVal[[f]]$'EZ'
    do.call('rbind', lapply(1:length(fitFoldPred), function(t){
        predT = fitFoldPred[[t]]
        foldID = yListFolds[[t]] ; y = yList[[t]]
        foldID[foldID!=f]=NA ; foldID[!is.na(foldID)] = 1
        y=y*foldID ; predT=predT*foldID ## change here for normal!!!
        res=na.omit(data.frame(actual=c(y), pred=c(predT), fold=f, stringsAsFactors=FALSE))
                                        #res$pred = 1/(1+exp(-res$pred)) --> normal model
        return(res) }) ) }) )
    
## get perf stats  (just do RMSE)
aucByFold=do.call('rbind', lapply(1:folds, function(f){
    slice = outPerf[outPerf$fold==f,]
    if(length(unique(slice$actual))==1){return(NULL) }
    perf=cbind(fold=f,
               aucROC=getAUC(slice$pred, slice$actual),
               aucPR=auc_pr(slice$actual, slice$pred)
    )
    return(perf) } ))
  aucROC=getAUC(outPerf$pred, outPerf$actual)
  aucPR=auc_pr(outPerf$actual, outPerf$pred)
  ################

  print("organizing output for return")
  # org output and return
  out=list(outPerf=outPerf, aucByFold=aucByFold,
      aucROC=aucROC, aucPR=aucPR)
 return(out)
}


################

################
## run outsamp models


## ameOutSamp_NULL = ameOutSamp(
##     yList=yList, xDyadL=NULL,
##     xRowL=NULL, xColL=NULL,
##     startVals=ameFit$'startVals')


## ameOutSamp_wFullSpec = ameOutSamp(
##   yList=yList, xDyadL=xDyadL, xRowL=xRowL, xColL=xColL,
##   startVals=fitFullSpec$startVals
## )
