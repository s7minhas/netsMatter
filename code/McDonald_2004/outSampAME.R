##This script for the AME out-sample
## difference appears to be in the fit function
## becomes the ameRepl function
## instead of the glm
################
# workspace
rm(list=ls())

## load script to load package
source('../weeks_2012/loadPackage.R')

library(dplyr)
library(magrittr)
library(reshape2)
library(tidyr)
################
## set up different data paths depending on whether on my computers or Dave's server
## load setup.R for small helper functions
##case 1: Dave's server:
if(Sys.info()['user']== 'margaret'){
    source('~/projects/netsmatter/code/netsMatter/code/McDonald_2012/setup.R')
    # data path named as "dpath"
}

## One of my machines
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
    source('setup.R')
}


################
## load ame results
## load the k-specific data:
## as well as the ame formulation of the data
##source('xValSetupK0.R') #started 11:41 am 6/24. note amen data is nonsymmetric
##source('xValSetupk1.R') #started 12:03 pm 6/24 data nonsymmetric
##source('xValSetupk2.R') #started 12:01 pm 6/24 data non-symmetric
source('xValSetupk3.R') #started at 12:04 5/24 data-non symetric

##FIX ME: figure out if need to rerun the AMEN models to be a symmetric model
## and then re-do the out-samp

print(paste0("Running with latDims= ", latDims))
print(paste0("running with burnin= ", burnin, "and nscans= ", nscans))
################

## all including DV
base_vars<- c('cw2mid', 'cw2midspl','cw2midsp1','cw2midsp2', 'cw2midsp3',
         'ally','cont1', 'lncaprat', 'ldep2l',
         'grow61l', 'lpolity42l','s_wt_glo', 'lrgdpch61h', 'lndistan',
              'majpow', 'limpduty0200h') %>%  strsplit(x = ., split =
                                                       " ") %>%  unlist()


form_mod = formula(paste0('value ~ ',
    paste(base_vars[-1], collapse = '+')))

form_mod

####################
ameOutSamp = function(
    yList, xDyadL=NULL, xRowL=NULL, xColL=NULL, startVals, ## startVal (to pick one portion)
    seed=6886, folds=30, #increase burn and nscan eventually
    R=latDims, model='bin',
    burn=1000, nscan=2000, odens=25, #expand when needed 
    intercept=FALSE, rvar=TRUE, cvar=TRUE, symmetric=FALSE){
    
    set.seed(6886)

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
            Y=ylist,
            Xdyad=xDyadL,
            Xrow=NULL,
            Xcol=NULL,
            symmetric=FALSE,
            rvar=rvar, cvar=cvar,
            R=latDims,
            model=model, intercept=intercept, seed=rseed,
            burn=burnin, nscan=nscans, odens=ods,
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
    ##source('../binPerf.R')
   
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
## run outsamp models


ylist <- yList
## need to figure out what's going on with this new
##"promise already under evaluation: recursive default argument reference ## or earlier problems?"  error.

ameOutSamp_NULL =   ameOutSamp(
    yList=ylist, xDyadL=xDyadList,
    xRowL=NULL,
    xColL=NULL,
    R=latDims,
    startVals=ameFit$startVals)

save(ameOutSamp_NULL, file=
                          paste0("outsampResults", latDims,".rda"))
## think that this is what I want to run, when I package
## the above back into a function
