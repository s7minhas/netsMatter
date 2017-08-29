rm(list=ls())
library(devtools) ; devtools::install_github('s7minhas/amen')
library(amen)
set.seed(6886)

# load data
load('~/Dropbox/Research/netsMatter/replications/example/inputData/exampleData.rda')

##################################################
# four fold cross validation
# for each year create a missmat, vary seed by year so we dont remove same country pairings
folds=3 ; set.seed(6886) ; seeds = sample(100:10000, length(yL)) ; print(seeds)
foldPosMat = lapply(1:length(yL), function(t){
	y = yL[[t]] ; seed = seeds[t]
	set.seed(6886) ; foldspos = sample(1:folds, length(y), replace=TRUE)
	posMat = matrix(foldspos, nrow=nrow(y), ncol=ncol(y))
	diag(posMat) = NA # dont waste crossval on diags
	return(posMat) })

# create set off missing and corresponding actual data
yMissBig=lapply(1:length(yL), function(t){
	y = yL[[t]] ; posMat = foldPosMat[[t]]
	yMiss = lapply(1:folds, function(x){ tmp=y ; tmp[which(posMat==x)]=NA ; return(tmp) })
	yAct = lapply(1:folds, function(x){ y[which(posMat==x)] })
	return(list(yMiss=yMiss, yAct=yAct)	) })

# reorg for modeling so we can loop through folds, # of null should match # of folds, ugly i know
foldAct = foldMiss = list(NULL, NULL, NULL)
for(k in 1:folds){ ; for(ii in 1:length(yMissBig)){
		foldMiss[[k]][[ii]] = yMissBig[[ii]]$yMiss[[k]]
		foldAct[[k]][[ii]] = yMissBig[[ii]]$yAct[[k]]
	} ; names(foldAct[[k]]) = names(foldMiss[[k]]) = names(yL) ; }
##################################################	

##################################################
# ame model params
imps = 1000
brn = 500
ods = 100
seed = 6886

########################
# run ame k=2 in parallel across folds
latDim = 2
library(doParallel) ; library(foreach)
cl=makeCluster(3) ; registerDoParallel(cl)

modsAme2 = foreach(ii=1:length(foldMiss), .packages=c('amen') ) %dopar% {
	dv = foldMiss[[ii]]
	ameFit = ame_repL(
		Y=dv,Xdyad=xDyadL,Xrow=xNodeL,Xcol=NULL, model="bin",symmetric=TRUE,
		R=latDim, 
		nscan=imps, seed=seed, burn=brn, odens=ods, 
		plot=FALSE, print=FALSE) 
	preds = unlist( lapply(1:length(ameFit$'EZ'), function(t){ ameFit$'EZ'[[t]][ which(foldPosMat[[t]]==ii) ] }) )
	prob = 1/(1+exp(-preds))
	pred = data.frame(prob=prob, actual=unlist( foldAct[[ii]] ) )
	return( pred=pred )
}
stopCluster(cl)
save(modsAme2, file=paste0('~/Dropbox/Research/netsMatter/replications/example/outputData/model_k',latDim,'_outPerfResults.rda'))
rm(list='modsAme2')
########################

########################
# run ame k=3 in parallel across folds
latDim = 3
library(doParallel) ; library(foreach)
cl=makeCluster(3) ; registerDoParallel(cl)

modsAme3 = foreach(ii=1:length(foldMiss), .packages=c('amen') ) %dopar% {
	dv = foldMiss[[ii]]
	ameFit = ame_repL(
		Y=dv,Xdyad=xDyadL,Xrow=xNodeL,Xcol=NULL, model="bin",symmetric=TRUE,
		R=latDim, 
		nscan=imps, seed=seed, burn=brn, odens=ods, 
		plot=FALSE, print=FALSE) 
	preds = unlist( lapply(1:length(ameFit$'EZ'), function(t){ ameFit$'EZ'[[t]][ which(foldPosMat[[t]]==ii) ] }) )
	prob = 1/(1+exp(-preds))
	pred = data.frame(prob=prob, actual=unlist( foldAct[[ii]] ) )
	return( pred=pred )
}
stopCluster(cl)
save(modsAme3, file=paste0('~/Dropbox/Research/netsMatter/replications/example/outputData/model_k',latDim,'_outPerfResults.rda'))
rm(list='modsAme3')
########################

########################
# run ame k=4 in parallel across folds
latDim = 4
library(doParallel) ; library(foreach)
cl=makeCluster(3) ; registerDoParallel(cl)

modsAme4 = foreach(ii=1:length(foldMiss), .packages=c('amen') ) %dopar% {
	dv = foldMiss[[ii]]
	ameFit = ame_repL(
		Y=dv,Xdyad=xDyadL,Xrow=xNodeL,Xcol=NULL, model="bin",symmetric=TRUE,
		R=latDim, 
		nscan=imps, seed=seed, burn=brn, odens=ods, 
		plot=FALSE, print=FALSE) 
	preds = unlist( lapply(1:length(ameFit$'EZ'), function(t){ ameFit$'EZ'[[t]][ which(foldPosMat[[t]]==ii) ] }) )
	prob = 1/(1+exp(-preds))
	pred = data.frame(prob=prob, actual=unlist( foldAct[[ii]] ) )
	return( pred=pred )
}
stopCluster(cl)
save(modsAme4, file=paste0('~/Dropbox/Research/netsMatter/replications/example/outputData/model_k',latDim,'_outPerfResults.rda'))
rm(list='modsAme4')
########################

########################
# run ame k=1 in parallel across folds
latDim = 1
library(doParallel) ; library(foreach)
cl=makeCluster(3) ; registerDoParallel(cl)

modsAme1 = foreach(ii=1:length(foldMiss), .packages=c('amen') ) %dopar% {
	dv = foldMiss[[ii]]
	ameFit = ame_repL(
		Y=dv,Xdyad=xDyadL,Xrow=xNodeL,Xcol=NULL, model="bin",symmetric=TRUE,
		R=latDim, 
		nscan=imps, seed=seed, burn=brn, odens=ods, 
		plot=FALSE, print=FALSE) 
	preds = unlist( lapply(1:length(ameFit$'EZ'), function(t){ ameFit$'EZ'[[t]][ which(foldPosMat[[t]]==ii) ] }) )
	prob = 1/(1+exp(-preds))
	pred = data.frame(prob=prob, actual=unlist( foldAct[[ii]] ) )
	return( pred=pred )
}
stopCluster(cl)
save(modsAme1, file=paste0('~/Dropbox/Research/netsMatter/replications/example/outputData/model_k',latDim,'_outPerfResults.rda'))
rm(list='modsAme1')
########################
##################################################