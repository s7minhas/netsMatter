################
## todo: break this up so that the doParallel error won't
## disallow automating the xval:
##make the xval take in parameters from a .R script
## then fit the model using doparallel and save the output
## then extracting the diagnostics in a separate script

# workspace
rm(list=ls())


  ################
  # divide dataset into folds
  set.seed(seed)
  yListFolds = lapply(yList, function(y){
    yFold=matrix(sample(1:folds, length(y), replace=TRUE),
                 nrow=nrow(y),ncol=ncol(y), dimnames=dimnames(y)) # Na --> affects folds, so only 4-5 folds
    diag(yFold) = NA
    return(yFold) }) # num of folds
  ################
  
  ################
  # run models by fold
  yCrossValTrain = lapply(1:folds, function(f){ # setup list of training dataset
    yListMiss = lapply(1:length(yList), function(t){ # going to YList
      foldID = yListFolds[[t]] ; y = yList[[t]]
      foldID[foldID==f]=NA ; foldID[foldID!=f]=1 ; y=y*foldID # foldID[foldID==f]=NA ; foldID[!is.na(foldID)]=1 ; y=y*foldID line 38 (normal model)
      return(y) })
    names(yListMiss) = names(yList)
    return(yListMiss) }) ; names(yCrossValTrain) = char(1:folds)
  
  # run ame by fold
  library(doParallel)
  library(foreach)
  cores = 3
  registerDoParallel(cores)
  
  fitCrossVal = foreach(i = 1:length(yCrossValTrain), .packages=c('amen')) %dopar% {
    fit=ame_repL(
      Y=yCrossValTrain[[i]],
      Xdyad=xDyadL, Xrow=xRowL, Xcol=xColL, ## make sure start value low (seting huge Net model to NA)
      symmetric=symmetric, rvar=rvar, cvar=cvar, R=R,
      model=model, intercept=intercept, seed=seed,
      burn=burn, nscan=nscan, odens=odens,
      plot=FALSE, gof=TRUE, periodicSave=FALSE,
      startVals=startVals )
    return(fit)
  }
  stopCluster(cores)

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
  
  # get perf stats  (just do RMSE)
  aucByFold=do.call('rbind', lapply(1:folds, function(f){
    slice = outPerf[outPerf$fold==f,]
    if(length(unique(slice$actual))==1){ return(NULL) }
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
  out=list(
    outPerf=outPerf, aucByFold=aucByFold,
    aucROC=aucROC, aucPR=aucPR
  )
  return(out)
}
################

################
# run outsamp models
ameOutSamp_NULL = ameOutSamp()

ameOutSamp_wFullSpec = ameOutSamp(
  yList=yList, xDyadL=xDyadL, xRowL=xRowL, xColL=xColL,
  startVals=fitFullSpec$startVals
)

# save
save(
  ameOutSamp_NULL, ameOutSamp_wFullSpec,
  file=paste0(pathResults, 'ameCrossValResults.rda')
)
################
