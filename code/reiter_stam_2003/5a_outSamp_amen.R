rm(list=ls())
################
resultsPath = '/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/'
fPth= '/Users/juanftellez/OneDrive/netsMatter/code/reiter_stam_2003/'
source(paste0(fPth, 'setup.r'))
source(paste0(fPth, 'binPerfHelpers.r'))
################
# load data
load(paste0(resultsPath, 'amenData.rda')) 
load(paste0(resultsPath, 'model_k0_v12.rda')); ameFit_k0 = ameFit
load(paste0(resultsPath, 'model_k1_v12.rda')); ameFit_k1 = ameFit
load(paste0(resultsPath, 'model_k2_v12.rda')); ameFit_k2 = ameFit
load(paste0(resultsPath, 'model_k3_v12.rda')); ameFit_k3 = ameFit
rm(ameFit)
################

################
# function to run k-fold cross validation analysis using ame
ameOutSamp = function(
  yList, xDyadL=NULL, xRowL=NULL, xColL=NULL, startVals,
  seed=6886, folds=30, # set folds = 4
  R=2, model='bin', burn=1000, nscan=2000, odens=10, 
  intercept=TRUE, rvar=TRUE, cvar=TRUE, symmetric=FALSE
){
  
  ################
  # divide dataset into folds
  set.seed(seed)
  yListFolds = lapply(yList, function(y){ # assign each y-matrix to a fold
    yFold=matrix(sample(1:folds, length(y), replace=TRUE), 
                 nrow=nrow(y),ncol=ncol(y), dimnames=dimnames(y))
    diag(yFold) = NA
    return(yFold) })
  ################
  
  ################
  # run models by fold
  yCrossValTrain = lapply(1:folds, function(f){
    yListMiss = lapply(1:length(yList), function(t){
      foldID = yListFolds[[t]] ; y = yList[[t]] # find all values for given fold
      foldID[foldID==f]=NA ; 
      foldID[!is.na(foldID)]=1 ; y=y*foldID # set all values for that fold = NA
      return(y) }) 
    names(yListMiss) = names(yList)
    return(yListMiss) }) ; names(yCrossValTrain) = char(1:folds)
  
  # run ame by fold
  fitCrossVal = lapply(yCrossValTrain, function(yCV){
    fit=ame_repL(
      Y=yCV, Xdyad=xDyadL, Xrow=xRowL, Xcol=xColL,
      symmetric=symmetric, rvar=rvar, cvar=cvar, R=R, 
      model=model, intercept=intercept, seed=seed,
      burn=burn, nscan=nscan, odens=odens, 
      plot=FALSE, gof=TRUE, periodicSave=FALSE,
      startVals=startVals
    )
    return(fit) })
  
  # get preds
  outPerf = do.call('rbind', lapply(1:folds, function(f){
    fitFoldPred = fitCrossVal[[f]]$'EZ'
    do.call('rbind', lapply(1:length(fitFoldPred), function(t){
      predT = fitFoldPred[[t]]
      foldID = yListFolds[[t]] ; y = yList[[t]]
      foldID[foldID!=f]=NA ; foldID[!is.na(foldID)] = 1
      y=y*foldID ; predT=predT*foldID
      res=na.omit(data.frame(actual=c(y), pred=c(predT), fold=f, stringsAsFactors=FALSE))
      res$pred = 1/(1+exp(-res$pred))
      return(res) }) ) }) )
  
  # get binperfhelpers
  loadPkg(c('ROCR', 'RColorBrewer', 'caTools'))
  source(paste0(fPth, 'binPerfHelper.r'))
  
  # get perf stats
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
  
  # org output and return
  out=list(
    fitCrossVal=fitCrossVal,
    outPerf=outPerf, aucByFold=aucByFold,
    aucROC=aucROC, aucPR=aucPR
  )	
  return(out)
}
################

ameOutSamp_k2 = ameOutSamp(
  yList=yList, xDyadL=xDyadList, xRowL=NULL, xColL=NULL,
  startVals=ameFit_k2$'startVals', folds = 30, 
  burn=2000, nscan=4000, odens=10
)
