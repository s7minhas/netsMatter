rm(list=ls())
library(devtools) ; devtools::install_github('s7minhas/amen')
library(amen)

load('~/Dropbox/Research/netsMatter/replications/example/inputData/exampleData.rda')

##############################
# run ame in parallel # mcmc params
imps = 1000
brn = 5
ods = 1
latDims = 1:4
seed=6886

# Run amen in parallel
library(doParallel) ; library(foreach)
cl=makeCluster(4) ; registerDoParallel(cl)
foreach(ii=1:length(latDims), .packages=c("amen")) %dopar% {
	
	fit = ame_repL(
		Y=yL,Xdyad=xDyadL,Xrow=xNodeL,Xcol=NULL, model="bin",symmetric=TRUE,
		R=latDims[ii], 
		nscan=imps, seed=seed, burn=brn, odens=ods, 
		plot=FALSE, print=FALSE) 
	
	save(fit, file=paste0('~/Dropbox/Research/netsMatter/replications/example/outputData/modelk',latDims[ii],'.rda'))
}
stopCluster(cl)
##############################