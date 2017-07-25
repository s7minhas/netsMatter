################
# workspace
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
 #pathData='~/Dropbox/netsMatter/replications/Weeks2012/replication/data/'
}

loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
	}
}

loadPkg(c('reshape2', 'ROCR', 'caTools', 'RColorBrewer'))

#library(devtools) ; devtools::install_github(‘s7minhas/amen’) ; library(amen)

loadPkg('devtools') ; devtools::install_github('s7minhas/amen') ; library(amen)

###

source('binPerf.R')
names(ameFit)


################
################

## load data

pathResults <- "~/Dropbox/netsMatter/replications/Weeks2012/replication/output/"

## previous data
load('~/Dropbox/netsMatter/replications/Weeks2012/replication/output/WeeksamenData.rda')

ls()
## AMN output
#load(paste0(pathResults, 'model_k12017-03-15_v2.rda'))
load(paste0(pathResults, 'model_k02017-03-15_v2.rda'))
#load(paste0(pathResults, 'model_k32017-03-14_v2.rda'))
#load(paste0(pathResults, 'model_k22017-04-04_v2.rda'))
################

## a different script for every model that they ran 

yList=yList
xDyadL=xDyadList
xRowL= xNodeList.s
xColL=nNodeList.r
startVals=ameFit$startVals
seeds=6886
folds=3
R=2
model='bin'
burn=10
nscan=50
odens=1
intercept=TRUE
rvar=TRUE
cvar=TRUE
symmetric=FALSE


################
# function to run k-fold cross validation analysis using ame

ameOutSamp = function(
    yList, xDyadL=NULL, xRowL=NULL,
    xColL=NULL, startVals, ## startVal (to pick one portion)
    seed=6886, folds=30,
    nscan=2000, odens=25, # burn=1000, nscan= 2000, odens=10, just save time starting from low
    intercept=TRUE, rvar=TRUE, cvar=TRUE, symmetric=FALSE
    ){

################
    ## going into every element of ylist
    ## creating a separate matrix of same dimes of Y,  fill
    ## in with same fold assignment
	# divide dataset into folds
	set.seed(seed)
	yListFolds = lapply(yList, function(y){
		yFold=matrix(sample(1:folds, length(y), replace=TRUE),
			nrow=nrow(y),ncol=ncol(y), dimnames=dimnames(y)) # Na --> affects folds, so only 4-5 folds
		diag(yFold) = NA
		return(yFold) })

        ls()
        class(yListFolds)

        length(yListFolds)
        
                                        # num of folds

        ##create a corresponding  matrix,
        ## assign fold values rather than data

        ## distribution should be approximately uniform
### then ignore diagonals
        ## remove all dyads in one fold, estimate model
        ## see if we can predict those dyads
	################

        ## outpef should look like actual and predicted values by fold
        ## then we go into perfstats and look at the folds
        ## make sure that there is something in the fold, combine
        ## by looking at AUC and RIC

        ## then save everything: xvalidated dataset; fitted values by fold
        ## AUC
                                        #
        ##one other change to make sure that the file size does not blow up:
        ## look at code, for a small number of nscan (10, 
	################
                                        # run models by fold
        ## goes into ylist object
        ## NA outs observations based on their fold assignment
        ## have to work with lapply
        ## loop through with time id to see what is going on:
        ## doing iteratively


        ## resulting structure:
        ## names =1, 2, 3 where each stands for a fold
        ## inside yCrossValTrain should be of length 50
        ## which corresponds to length of matricies in the data

        ## one thing that I can do, create a foreach block  in doParallel
        ## fitcrossval=foreach(i=1:length(yCrossValTrain)) % .packages=c('amen')) dopar$ {}
        ## wrap into a parallel process 
        ## goes into first list, and selects out corresponding time periods

        yCrossValTrain = lapply(1:folds, function(f){ # setup list of training dataset
		yListMiss = lapply(1:length(yList), function(t){ # going to YList
                    foldID = yListFolds[[t]] ; y = yList[[t]] ## selects out element from DV

                    ## this tells us what to NA out; and actual data
			foldID[foldID==f]=NA ; y=y*foldID # foldID[foldID==f]=NA ; foldID[!is.na(foldID)]=1 ; y=y*foldID line 38 (normal model)
                    return(y) })

                ## want to take from this: for every fold ID if the fold ID is equal to
                ## the one that we're interested in, make it NA.
                ## this makes next step simple== to make observations missing in
                ## actual dataset, makes that observation NA in data.

                ## in non-binary data, would multiply some number by.
                ## if you specify a binary DV, will automatically make an entry greater than
                ## i into 1. 
		names(yListMiss) = names(yList)
		return(yListMiss) }) ; names(yCrossValTrain) = char(1:folds)

        ## the above sets generate a ylist, ylist miss
        ## and the bigger apply does this for every fold
        ## code returns

        
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
	loadPkg(c('ROCR', 'RColorBrewer', 'caTools'))
	source(paste0(fPth, 'binPerfHelpers.R'))

	# get perf stats  (just do RMSE)
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
		yCrossValTrain=yCrossValTrain,
		fitCrossVal=fitCrossVal,
		outPerf=outPerf, aucByFold=aucByFold,
		aucROC=aucROC, aucPR=aucPR
	)
	return(out)
    }

ls()
################
## only thing need to change at each in run in fitCrossVal
## reason to include startvals is to

################
# run outsamp models
ameOutSamp_NULL = ameOutSamp(
	yList=yList, xDyadL=NULL, xRowL=NULL, xColL=NULL,
	startVals=fit$startVals
	)

ameOutSamp_wFullSpec = ameOutSamp(
	yList=yList, xDyadL=xDyadL, xRowL=xRowL, xColL=xColL,
	startVals=fitFullSpec$startVals
	)

# save
save(
	ameOutSamp_NULL, ameOutSamp_wFullSpec,
	file=paste0(pathResults, 'ameCrossValResults.rda')
	)
################
