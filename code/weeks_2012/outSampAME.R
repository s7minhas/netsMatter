##This script for the AME out-sample
## difference appears to be in the fit function
## becomes the ameRepl function
## instead of the glm


################
# workspace
rm(list=ls())

## load script to load package
source('LoadPackage.R')
#source('setup.R') #for some small helper functions

char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

library(dplyr)
library(reshape2)
library(tidyr)
################
path = '~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'
################
# load data
load(paste0(path, 'WeeksamenData.rda'))

# crossval params
seed=6886
folds=30

ls()

################

dyadicVars <- c('mzinit', "dependlow" , "majmaj", "minmaj", "majmin",
                "contigdum", "logdist","s_wt_glo","pcyrsmzinit",
                "pcyrsmzinits1", "pcyrsmzinits2", "pcyrsmzinits3") %>%  strsplit(x = ., split = " ") %>%  unlist()


base_vars= ## all, inc DV as the first
    c("mzinit", 'machinejlw_1', 'juntajlw_1', 'bossjlw_1',
      'strongmanjlw_1','allotherauts_1', 'newregime_1', 'democracy_2',
      'cap_1', 'cap_2', 'initshare', 'dependlow', 'majmaj', 'minmaj',
      'majmin','contigdum','logdist', 's_wt_glo', 's_lead_1',
      's_lead_2','pcyrsmzinit','pcyrsmzinits1', 'pcyrsmzinits2',
      'pcyrsmzinits3') %>%  strsplit(x = ., split = " ") %>%
    unlist()

form_mod = formula(paste0('value ~ ',
    paste(base_vars[-1], collapse = '+')))

form_mod

ls()

################

ameOutSamp = function(
	yList, xDyadL=NULL, xRowL=NULL, xColL=NULL, startVals, ## startVal (to pick one portion)
	seed=6886, folds=30, #increase burn and nscan eventually
    R=2, model='bin', burn=1000, nscan=200, odens=25, 
    intercept=TRUE, rvar=TRUE, cvar=TRUE, symmetric=FALSE){
    
    set.seed(seed)

    ## Helper function #1:
    ## Divide dataset into folds
    yListFolds = lapply(yList, function(y){
        yFold=matrix(sample(1:folds, length(y), replace=TRUE),
            nrow=nrow(y),ncol=ncol(y), dimnames=dimnames(y))
        diag(yFold) = NA
        return(yFold) }) # num of folds
    
#####
    
    yCrossValTrain = lapply(1:folds, function(f){ # setup list of training dataset
        yListMiss = lapply(1:length(yList), function(t){ # going to YList
            foldID = yListFolds[[t]] ; y = yList[[t]]
            foldID[foldID==f]=NA ; y=y*foldID 
            return(y) })
        names(yListMiss) = names(yList)
        return(yListMiss) }) ; names(yCrossValTrain) =
            char(1:folds)
    
    ## run AME
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
    
    ## get preds (predictions)
    outPerf = do.call('rbind', lapply(1:folds, function(f){
        fitFoldPred = fitCrossVal[[f]]$'EZ'
        do.call('rbind', lapply(1:length(fitFoldPred), function(t){
            predT = fitFoldPred[[t]]
            foldID = yListFolds[[t]] ; y = yList[[t]]
            foldID[foldID!=f]=NA ; foldID[!is.na(foldID)] = 1
            y=y*foldID ; predT=predT*foldID ## change here for normal!!!
            res=na.omit(data.frame(actual=c(y), pred=c(predT), fold=f, stringsAsFactors=FALSE))
            ##res$pred = 1/(1+exp(-res$pred)) --> normal model
            return(res) }) ) }) )
    
    ## Helper function #4
    ## get binperfhelpers
    loadPkg(c('ROCR', 'RColorBrewer', 'caTools'))
    source('binPerf.R')
   
    ## helper function #5:
    ## get performance stats    
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
    
    ##Format to Return
    out = list( yCrossValTrain=yCrossValTrain,
        fitCrossVal=fitCrossVal,
        outPerf=outPerf, aucByFold=aucByFold,
        aucROC=aucROC, aucPR=aucPR )
    
    return(out)
}
###############
################
# run outsamp models
ameOutSamp_NULL = ameOutSamp(
	yList=yList, xDyadL=NULL, xRowL=NULL, xColL=NULL,
	startVals=fit$startVals
	)

## think that this is what I want to run, when I package
## the above back into a function
