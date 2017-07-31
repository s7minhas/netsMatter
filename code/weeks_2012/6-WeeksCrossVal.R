################
# workspace
rm(list=ls())

##source('xValSetupk2.R') #finished 4/21
##source('xValSetupk3.R')  #finished 4/22

##source('xValSetupk1.R') #finished 4/25

source('xValSetupK0.R')

################

################
## divide dataset into folds
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


######################
##Run AME by Fold
#######################

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


save(fitCrossVal,
     file=paste0(path, 'fitCrossVal_k', R,'.r'))
