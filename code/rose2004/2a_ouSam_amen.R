################
rm(list=ls())

if(Sys.info()['user']=='howardliu'){
  source('~/netsMatter/code/rose2004/loadPkg.R')
  pathResults = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/"
}
if(Sys.info()['user']=='s7m'){
  source('~/Research/netsMatter/code/rose2004/loadPkg.R')
  pathDrop = '~/Dropbox/Research/netsMatter/'
  pathResults = paste0(pathDrop, 'replications/rose2004/')
} 
################

# load pkg
loadPkg=function(toLoad){
    for(lib in toLoad){
      if(!(lib %in% installed.packages()[,1])){
        install.packages(lib, repos='http://cran.rstudio.com/') }
      suppressMessages( library(lib, character.only=TRUE) ) } }
packs = c('foreign','dplyr', 'ggplot2', 'readr', 'lmtest','multiwayvcov', 'amen')
loadPkg(packs)

# some short-handed functions
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

################
# load data
load(paste0(pathResults, 'amenData_rose.rda'))

# load(paste0(resultsPath, 'ameFit_k3_v1.rda')); ameFit_k3 = ameFit

# load(paste0(resultsPath, 'ameFit_k2_v1.rda')); ameFit_k2 = ameFit

# load(paste0(resultsPath, 'ameFit_k1_v1.rda')); ameFit_k1 = ameFit ##??

# load(paste0(resultsPath, 'ameFit_k0_v1.rda')); ameFit_k0 = ameFit

load(paste0(pathResults, 'outputData/ameFit_k2_v2_imps_50000_intercept.rda')); ameFit_k2 = ameFit
################

################
# function to run k-fold cross validation analysis using ame
ameOutSamp = function(
  yList, xDyadL=NULL, xRowL=NULL, xColL=NULL, startVals, ## startVal (to pick one portion)
  seed=6886, folds=30,
  R=2, model='nrm', burn=10000, nscan=2000, odens=25, # burn=1000, nscan= 2000, odens=10, just save time starting from low
  intercept=TRUE, rvar=TRUE, cvar=TRUE, symmetric=FALSE
){

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
      foldID[foldID==f]=NA ;foldID[!is.na(foldID)]=1; y=y*foldID # foldID[foldID==f]=NA ; foldID[!is.na(foldID)]=1 ; y=y*foldID line 38 (normal model)
      return(y) })
    names(yListMiss) = names(yList)
    return(yListMiss) }) ; names(yCrossValTrain) = char(1:folds)

  # run ame by fold
  fitCrossVal = lapply(yCrossValTrain, function(yCV){
    fit=ame_repL(
      Y=yCV, Xdyad=xDyadL, Xrow=xRowL, Xcol=xColL, ## make sure start value low (seting huge Net model to NA)
      symmetric=symmetric, rvar=rvar, cvar=cvar, R=R,
      model=model, intercept=intercept, seed=seed,
      burn=burn, nscan=nscan, odens=odens,
      plot=FALSE, gof=TRUE, periodicSave=FALSE,
      startVals=startVals
    )
    return(fit) })

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

  # get binperfhelpers
  rmse = sqrt(mean((outPerf$actual - outPerf$pred)^2, na.rm = T))
  rmdse = sqrt(median((outPerf$actual - outPerf$pred)^2, na.rm = T))
  ssTOT = sum((outPerf$actual - mean(outPerf$actual, na.rm = T))^2, na.rm = T)
  ssRES = sum((outPerf$pred - outPerf$actual)^2, na.rm = T)
  rmseByFold=do.call('rbind', lapply(1:folds, function(f){
    slice = outPerf[outPerf$fold==f,]
    if(length(unique(slice$actual))==1){ return(NULL) }
    perf=cbind(fold=f,
               rmse=sqrt(mean((slice$actual - slice$pred)^2, na.rm = T)),
               rmdse =sqrt(median((slice$actual - slice$pred)^2, na.rm = T)),
               r2 =1 - sum((slice$pred - slice$actual)^2, na.rm = T)/sum((slice$actual - mean(slice$actual, na.rm = T))^2, na.rm = T))
    return(perf) } ))

  r2 = 1 - ssRES/ssTOT
  out = list( yCrossValTrain=yCrossValTrain,
              fitCrossVal=fitCrossVal,
              outPerf=outPerf, rmseByFold=rmseByFold,
              rmseOUT = rmse, rmdseOUT = rmdse )
  return(out)
}
################

################
# run outsamp models

# k =2 
ameOutSamp_k2 = ameOutSamp(
  yList=yList, xDyadL=xDyadList, xRowL=NULL, xColL=NULL,
  startVals=ameFit_k2$'startVals',
  folds = 30,
  burn=2000, nscan=4000, odens=10
) # take 3 hours

ameOutSamp = ameOutSamp_k2
save(ameOutSamp, file=paste(pathResults, 'outputData/ameCrossvalResults_k2_v2_imps_50000_intercept_30folds.rda'))

# # k = 3
# ameOutSamp_k3 = ameOutSamp(
#   yList=yList, xDyadL=xDyadList, xRowL=NULL, xColL=NULL,
#   startVals=ameFit_k3$'startVals', # k = 3
#   folds = 4,
#   burn=2000, nscan=4000, odens=10
# )
# # save
# save(ameOutSamp_k3,
#   file=paste0(pathResults, 'ameCrossValResults_k3.rda')
# )

# ameOutSamp_k3$rmseOUT #  1.967908

# # k = 2
# ameOutSamp_k2 = ameOutSamp(
#   yList=yList, xDyadL=xDyadList, xRowL=NULL, xColL=NULL,
#   startVals=ameFit_k2$'startVals',
#   folds = 4,
#   burn=2000, nscan=4000, odens=10
# ) # take 3 hours

# ameOutSamp_k2$rmseOUT # 1.987901 

