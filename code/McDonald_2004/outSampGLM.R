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

source(paste0(scriptPath, "LoadPackage.R"))

library(dplyr)
library(reshape2)
library(tidyr)
################

dataPath = '~/Dropbox/netsMatter/replications/McDonald_2004/data/'
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
              'majpow', 'limpduty0200h') %>%  strsplit(x = ., split =
                                                       " ") %>%  unlist()

ids = c('ccode1', 'ccode2', 'year', 'dyadid')

form_mod = formula(paste0('value ~ ',
    paste(base_vars[-1], collapse = '+')))

form_mod


################

#glmOutSamp = function(glmForm){
    
set.seed(seed)
    ## Helper function #1:
    ## Divide dataset into folds
    
yListFolds = lapply(yList, function(y){
    yFold=matrix(sample(1:folds, length(y), replace=TRUE),
        nrow=nrow(y),ncol=ncol(y), dimnames=dimnames(y))
    diag(yFold) = NA
    return(yFold) })

## helper function #2:
#### run models by fold

##glmOutSamp = function(glmForm){
	################
## divide dataset into folds
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
                foldID = yListFolds[[t]]
                y = yList[[t]]
                foldID[foldID==f]=NA
                y=y*foldID
                return(y) })

            names(yListMiss) = names(yList)
            return(yListMiss) }) ; names(yCrossValTrain) = as.character(1:folds)

class(yCrossValTrain)
## step through the melt into glm format
## fold by fold

y=yCrossValTrain[[1]]

class(y) ## list, entry is for years
attributes(y)

	## melt into glm format
	###yCrossValTrain = lapply(yCrossValTrain, function(y){
          ##  print("**")
            y = melt(y) ## turns list into a df with vars of dyad ids, value, and L1= year
xd = melt(xDyadList)## makes a year list into a dyad data-frame
xd = tidyr::spread(xd, key = Var3, value = value) ## independent variables by dyad, 18k x 18
##print(head(xd))
glmData = cbind(y,xd[,base_vars[-1]]) 
glmData = glmData[which(glmData$Var1!=glmData$Var2),]
## return(glmData)
##	})

head(glmData)

names(yCrossValTrain) = as.character(1:folds)
print(names(yCrossValTrain))


        ## helper function #3:
    ## run glm
    
    fitCrossVal = lapply(yCrossValTrain, function(glmData){
        print("*")
        glmData$value[glmData$value>1] = 1
        fit = glm(form_mod, data=glmData, family='binomial')	
        return(fit)
    })

attributes(fitCrossVal)
class(fitCrossVal)

    ## get predictions
    
    #outPerf = do.call('rbind', lapply(1:folds, function(f){
        ## print counter
     #   print("#")
## get probs
f=1 #for the debugging

testData =# leading 1s, then 
    cbind(int=1,yCrossValTrain[[f]][is.na(yCrossValTrain[[f]]$value),
              names(coef(fitCrossVal[[f]]))[-1]]) 
prob = 1/(1+exp(-as.matrix(testData) %*% coef(fitCrossVal[[f]])))
                                        #Logit link

class(testData)
        ## print(head(prob))
        
## get actual
## MJF: parse through "actual" to debug it:
## first line: for each entry in yListFolds:

actual=unlist(lapply(1:length(yListFolds), function(t){
    
    foldID = yListFolds[[t]] ; y = yList[[t]]
    foldID[foldID!=f]=NA ; foldID[!is.na(foldID)] = 1
    y=c(y*foldID) ; return(y[!is.na(y)])
}))


        if(length(actual)!=length(prob)){stop('shit went wrong.')}
        res = data.frame(actual=actual, pred=prob, fold=f,
            stringsAsFactors = FALSE)
        
        ##if(any(grepl('lagDV',glmOutSamp))){res=na.omit(res)}
        return(res)
    }))
    
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

form_mod
ls()

## think that this is what I want to run, when I package
## the above back into a function

glmOutSamp_wFullSpec=glmOutSamp(glmForm=form_mod)

##save current work:

## too large to save:
##save(glmOutSamp_wFullSpec, file=paste0(dataPath, "McDGLMPerf.rda"))


ls() 
