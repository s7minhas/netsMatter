##This script for the GLM comparison 

## Plan on 5/10:
## First run GLM with just the dyadic vars
## then add in each of the sender and reciever
## vars by merging them into the
## xd data.frame

################
# workspace
rm(list=ls())

## load script to load package
scriptPath <- "../weeks_2012/"
scriptPath <- '~/Research/netsMatter/code/weeks_2012/'

source(paste0(scriptPath, "LoadPackage.R"))
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }

library(dplyr)
library(reshape2)
library(tidyr)
################

dataPath = '~/Dropbox/netsMatter/replications/McDonald_2004/data/'
dataPath = '~/Dropbox/Research/netsMatter/replications/McDonald_2004/data/'
################
# load data

load(paste0(dataPath, 'McDonald_baseModel.rda'))
load(paste0(dataPath, "amenData.rda"))
ls()

McDdata = foreign::read.dta(paste0(dataPath, 'PTTOFTfvs.dta'))

# crossval params
seed=6886
folds=30
################

## remember: no dyadic vars, so all are
## base vars:

## notes on vars: cw2midspl, cw2midsp1, cw2midsp2, cw2midsp3 are all splines
## ally is binary for any alliance type;
## lpolity42l is a lagged democracy variable, lower value of the lagged democracy var in the dyad
## ldep2l is lower dependency variable of dyad
## limpduty0200h is higher tarrif barrier of two states

## all including DV
base_vars<- c('cw2mid', 'cw2midspl','cw2midsp1','cw2midsp2', 'cw2midsp3',
         'ally','cont1', 'lncaprat', 'ldep2l',
         'grow61l', 'lpolity42l','s_wt_glo', 'lrgdpch61h', 'lndistan',
              'majpow', 'limpduty0200h') %>%
    strsplit(x = ., split =" ") %>%  unlist()
ids = c('ccode1', 'ccode2', 'year', 'dyadid')

form_mod = formula(paste0('value ~ ', paste(base_vars[-1], collapse = '+')))
glmForm = form_mod
################

##glmOutSamp = function(glmForm){
set.seed(seed)
    ################
    # divide dataset into folds
    set.seed(seed)
    yListFolds = lapply(yList, function(y){
        yFold=matrix(sample(1:folds, length(y), replace=TRUE),
            nrow=nrow(y),ncol=ncol(y), dimnames=dimnames(y))
        diag(yFold) = NA
        return(yFold) })
    ################

    ################
    # run models by fold
    yCrossValTrain = lapply(1:folds, function(f){
        yListMiss = lapply(1:length(yList), function(t){
            foldID = yListFolds[[t]] ; y = yList[[t]]
            foldID[foldID==f]=NA ; y=y*foldID 
            return(y) })
        names(yListMiss) = names(yList)
        return(yListMiss) }) ; names(yCrossValTrain) = char(1:folds)

    # melt into glm format
    yCrossValTrain = lapply(yCrossValTrain, function(y){
        y = melt(y)
        xd = dcast(melt(xDyadList), Var1 + Var2 + L1 ~ Var3)
        xd = xd[order(xd$L1, xd$Var2, xd$Var1),]

        glmData = cbind(y,xd)
        glmData = glmData[which(glmData$Var1!=glmData$Var2),]
        return(glmData) 
    })

    # run glm
    fitCrossVal = lapply(yCrossValTrain, function(glmData){
        glmData$value[glmData$value>1] = 1
        fit = glm(glmForm, data=glmData, family='binomial') 
        return(fit)
    })

    # get preds
    outPerf = do.call('rbind', lapply(1:folds, function(f){
        # get probs
        testData = cbind(int=1,yCrossValTrain[[f]][is.na(yCrossValTrain[[f]]$value),names(coef(fitCrossVal[[f]]))[-1]])
        testData = na.omit(testData)
        prob = 1/(1+exp(-as.matrix(testData) %*% coef(fitCrossVal[[f]])))

        # get actual
        actual=unlist(lapply(1:length(yListFolds), function(t){
                foldID = yListFolds[[t]] ; y = yList[[t]]
                foldID[foldID!=f]=NA ; foldID[!is.na(foldID)] = 1
                y=c(y*foldID) ; return(y[!is.na(y)])
            }))
        if(length(actual)!=length(prob)){stop('shit went wrong.')}
        res = data.frame(actual=actual, pred=prob, fold=f, stringsAsFactors = FALSE)
        return(res)
    }))
        
    # get binperfhelpers
    loadPkg(c('ROCR', 'RColorBrewer', 'caTools'))
    source(paste0(scriptPath, 'binPerf.R'))

    # get perf stats
    aucByFold=do.call('rbind', lapply(1:folds, function(f){
        slice = outPerf[outPerf$fold==f,]
        slice = na.omit(slice)
        if(length(unique(slice$actual))==1){ return(NULL) }
        perf=cbind(fold=f,
            aucROC=getAUC(slice$pred, slice$actual),
            aucPR=auc_pr(slice$actual, slice$pred)
            )
        return(perf) } ))
    aucROC=getAUC(outPerf$pred, outPerf$actual)
    aucPR=auc_pr(outPerf$actual, outPerf$pred)
    ################

    ################
    out = list( yCrossValTrain=yCrossValTrain,
        fitCrossVal=fitCrossVal,
        outPerf=outPerf, aucByFold=aucByFold,
        aucROC=aucROC, aucPR=aucPR )
    return(out)
    ################    
}
###############

## think that this is what I want to run, when I package
## the above back into a function

glmOutSamp_wFullSpec=glmOutSamp(glmForm=form_mod)

##save current work:

## too large to save:
##save(glmOutSamp_wFullSpec, file=paste0(dataPath, "McDGLMPerf.rda"))


ls() 
