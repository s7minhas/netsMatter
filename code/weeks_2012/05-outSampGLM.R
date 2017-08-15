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
source('loadPackage.R')

toLoad <- c("dplyr", "reshape2", "tidyr")

loadPkg(toLoad)

################
path = '~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'
################
# load data
load(paste0(path, 'WeeksamenData.rda'))

## crossval params
seed=6886
folds=4
################

dyadicVars <- c('mzinit', "dependlow" , "majmaj", "minmaj", "majmin", "contigdum","logdist","s_wt_glo","pcyrsmzinit", "pcyrsmzinits1", "pcyrsmzinits2","pcyrsmzinits3")  %>%  strsplit(x = ., split = " ") %>%
    unlist()

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


glmOutSamp = function(glmForm){
    
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
    
    yCrossValTrain = lapply(1:folds, function(f){
        yListMiss = lapply(1:length(yList), function(t){
            foldID = yListFolds[[t]]
            y = yList[[t]]
            foldID[foldID==f]=NA
            y=y*foldID
            return(y) })
        
        names(yListMiss) = names(yList)
        return(yListMiss) })
    
    names(yCrossValTrain) = as.character(1:folds)

    #### melt into glm format
    
    ## #xd = xd[order(xd$L1, xd$Var2, xd$Var1),]
    
    ## head(xd)
    
    yCrossValTrain = lapply(yCrossValTrain, function(y){
        ## this is for every entry in the yCrossValTrain
        ## list, which means that it works by year
        ## so I can merge the dyadic, sender, and rec
        
        ## print a counter
        print("**")
        ## covariates
        
        y = melt(y)
        ## dyadic
        
        ## L1 are the names of each entry in the list elements
        ## which is the years
        
        ##pull out the dyadic variables for each year
        xd = dcast(melt(xDyadList), Var1 + Var2 + L1 ~ Var3)
        
        ##rename to make clear that var1 is sender, var2 is reciever
        xd = xd[order(xd$L1, xd$Var2, xd$Var1),]
        
        
        ## pull out the sender variables for each year
        ## L indexe by list entries
        xr = dcast(melt(xNodeList.s), Var1 + L1 ~ Var2)
        head(xr)
        
    ## pull out reciever variables
        xc = dcast(melt(xNodeList.r), Var1 + L1 ~ Var2)
        
        glmData = cbind(y,xd)
        
        for(v in names(xc)[3:ncol(xc)] ){
            glmData$tmp = xc[,v][match(paste0(glmData$Var2,glmData$L1),
            paste0(xc$Var1,xc$L1))]
            names(glmData)[ncol(glmData)] = paste0(v, '.col')
        }
        for(w in names(xr)[3:ncol(xr)]){
            glmData$tmp = xr[,w][match(paste0(glmData$Var1,glmData$L1),
                paste0(xr$Var1,xr$L1))]
            names(glmData)[ncol(glmData)] = paste0(w, '.row')
        }
        
        glmData = glmData[which(glmData$Var1!=glmData$Var2),]
        
        glmData = glmData[which(glmData$Var1!=glmData$Var2),]
        return(glmData)
    })
    
    
    ## here we adjust the model forumula to account for the
    ## relabelig of the .col and .row variables
    ## (needed above or it doesn't subset correctly)
    form_mod2 <- formula(paste0('value ~ ',
                                paste(colnames(yCrossValTrain[[1]])[8:29]
                                      , collapse = '+')))
    
    

    ## helper function #3:
    ## run glm
    
    fitCrossVal = lapply(yCrossValTrain, function(glmData){
        print("*")
        glmData$value[glmData$value>1] = 1
        fit = glm(form_mod2, data=glmData, family='binomial')	
        return(fit)
    })

    
    ## get predictions
    
    outPerf = do.call('rbind', lapply(1:folds, function(f){
        ## print counter
        print("*")
        ## get probs
        testData =
            cbind(int=1,yCrossValTrain[[f]][is.na(yCrossValTrain[[f]]$value),names(coef(fitCrossVal[[f]]))[-1]]) 
        prob = 1/(1+exp(-as.matrix(testData) %*% coef(fitCrossVal[[f]])))
        
        ## get actual
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


## think that this is what I want to run, when I package
## the above back into a function

glmOutSamp_wFullSpec=glmOutSamp(glmForm=form_mod)

print('glm out samp run')
##save current work:

smallGLMoutSamp <- list(glmOutSamp_wFullSpec$aucROC, glmOutSamp_wFullSpec$aucPR)

names(smallGLMoutSamp) <- c("GLMaucROC", "GLMaucPR" )

## too large to save:
print("saving full out samp")

savePath <- '~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'

save(glmOutSamp_wFullSpec, file=paste0(savePath, "weeksOutPerf.rda"))

print("saving small out samp")
save(smallGLMoutSamp,file=paste0(savePath, "smallGLMoutSamp.rda"))
