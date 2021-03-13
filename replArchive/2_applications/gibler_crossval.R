#############################
# set a path
require(here)
pth = paste0(here::here(), '/replArchive/')
fPth = paste0(pth, 'helpers/')
gPth = paste0(pth, '2_applications/application_data/gibler/')

# install github from specific repo
# to get same results
# devtools::install_github('s7minhas/amen', ref='pa2018_version')
library(amen)

# some necessary libs
source(paste0(fPth, 'loadPkg.R'))
loadPkg(c(
  'reshape2', # data management
  'doParallel', 'foreach' # parallel
  ))

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

#
loadPkg(c('ROCR', 'RColorBrewer', 'caTools'))
source(paste0(fPth, 'binPerfHelpers.R'))
#############################

#############################
# load data
load(paste0(gPth, 'giblerData.rda'))
load(paste0(gPth, 'ameFitGibler.rda'))
#############################

#############################
# function to run k-fold cross validation analysis using ame
ameOutSamp = function(
  yList, xDyadL=NULL, xRowL=NULL, xColL=NULL, startVals,
  seed=6886, folds=4, cores=folds,
  R=2, model='bin', burn=1000, nscan=2000, odens=10,
  intercept=TRUE, rvar=TRUE, cvar=TRUE, symmetric=TRUE
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
  cl=makeCluster(cores) ; registerDoParallel(cl)
  fitCrossVal = foreach(yCV = yCrossValTrain, .packages=c('amen')) %dopar% {
    fit=ame_repL(
      Y=yCV, Xdyad=xDyadL, Xrow=xRowL, Xcol=xColL,
      symmetric=symmetric, rvar=rvar, cvar=cvar, R=R,
      model=model, intercept=intercept, seed=seed,
      burn=burn, nscan=nscan, odens=odens,
      plot=FALSE, gof=TRUE, periodicSave=FALSE,
      startVals=startVals
    )
    return(fit) } ; stopCluster(cl)

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
#############################

#############################
# run and save
ameOutSamp_k2 = ameOutSamp(
  yList=yList, xDyadL=xDyadList, xRowL=NULL, xColL=NULL,
  startVals=ameFit$'startVals', folds=4,  cores=4,
  burn=2000, nscan=4000, odens=10
)

save(ameOutSamp_k2, file=paste0(gPth, 'ameCrossVal_k2.rda'))
#############################
