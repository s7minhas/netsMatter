##This script for the AME out-sample
## difference appears to be in the fit function
## becomes the ameRepl function
## instead of the glm
################
# workspace
rm(list=ls())

## load script to load package
source('loadPackage.R')

library(dplyr)
library(reshape2)
library(tidyr)
################
## set up different data paths depending on whether on my computers or Dave's server
## load setup.R for small helper functions
##case 1: Dave's server:
if(Sys.info()['user']== 'margaret'){
    source('~/projects/netsmatter/code/netsMatter/code/weeks_2012/setup.R')
    # data path named as "dpath"
}

## One of my machines
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
    source('~/Dropbox/netsMatter/replications/Weeks2012/setup.R')
}


################
## load ame results
## load the k-specific data:
## as well as the ame formulation of the data
##source('xValSetupK0.R') #started 5:20 6/22
source('xValSetupk1.R') #started 5:30 pm  6/22
##source('xValSetupk2.R') #started 5:28 pm 6/22
##source('xValSetupk3.R') #started 5:19 6/22

## load helpers
source('binPerf.R') #should be in same directory

# crossval params
seed=6886
folds=30
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

################

## weeks-specific fix
rZ_bin_fc2 <- function(Z,EZ,rho,Y) {
    ## simulates Z under the contraints
    ## (1)  Y[i,j]=1   => Z[i,j]>0
    ##(2)  Y[i,j]=0   => Z[i,j]<0
   
    sz<-sqrt(1-rho^2)
    ut<-upper.tri(EZ)
    lt<-lower.tri(EZ)
    
    Y[is.na(Y)]<- -1
    for(y in c((-1):1))
    {
        lb<-c(-Inf,-Inf,0)[y+2] ; ub<-c(Inf,0,Inf)[y+2]

        up<- ut & Y==y
        ez<- EZ[up] + rho*( t(Z)[up]  - t(EZ)[up] )
        lbUnif <- pnorm((lb-ez)/sz)
        ubUnif <- pnorm((ub-ez)/sz)
        unif <- runif(sum(up),lbUnif,ubUnif)
        unif[unif==1]=1-(1e-16)
        Z[up]<-ez+sz*qnorm(unif)

        up<- lt & Y==y
        ez<- EZ[up] + rho*( t(Z)[up]  - t(EZ)[up] )
        lbUnif <- pnorm((lb-ez)/sz)
        ubUnif <- pnorm((ub-ez)/sz)
        unif <- runif(sum(up),lbUnif,ubUnif)
        unif[unif==1]=1-(1e-16)
        Z[up]<-ez+sz*qnorm(unif)
    }

    diag(Z)<-rnorm(nrow(Z),diag(EZ),1)
    Z
}

assignInNamespace("rZ_bin_fc", rZ_bin_fc2, pos="package:amen")

#####
ameOutSamp = function(
	yList, xDyadL=NULL, xRowL=NULL, xColL=NULL, startVals, ## startVal (to pick one portion)
	seed=6886, folds=30, #increase burn and nscan eventually
    R=1, model='bin', burn=1000, nscan=20000, odens=25, #expand when needed 
    intercept=TRUE, rvar=TRUE, cvar=TRUE, symmetric=FALSE){
    
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
## run outsamp models


ylist <- yList
## need to figure out what's going on with this new
##"promise already under evaluation: recursive default argument reference ## or earlier problems?"  error.

ameOutSamp_NULL =   ameOutSamp(
    yList=ylist, xDyadL=xDyadList,
    xRowL=xNodeList.s,
    xColL=xNodeList.r,
    R=1,
    startVals=ameFit$startVals)

save(ameOutSamp_NULL, file=
                          paste0("outsampResults", R,".rda"))
## think that this is what I want to run, when I package
## the above back into a function
