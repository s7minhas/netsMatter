################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
loadPkg('devtools') ; devtools::install_github('s7minhas/amen') ; library(amen)
################

################
# load data
load(paste0(pathResults, 'ameResults.rda'))
################

################
# function to run k-fold cross validation analysis using ame
ameOutSamp = function(
	yList, xDyadL=NULL, xRowL=NULL, xColL=NULL, startVals, ## startVal (to pick one portion)
	seed=6886, folds=30,
	R=2, model='bin', burn=10000, nscan=2000, odens=25, # burn=1000, nscan= 2000, odens=10, just save time starting from low
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
			foldID[foldID==f]=NA ; y=y*foldID # foldID[foldID==f]=NA ; foldID[!is.na(foldID)]=1 ; y=y*foldID line 38 (normal model)
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
################

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
