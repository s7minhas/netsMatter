if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/netsMatter/code/mansfield_milner_2012/setup.R') }

# load amen data
load( paste0(dataPath, 'amenData.rda') )

# running in parallel varying k
imps = 500000
brn = 250000
ods = 25
latDims = 0:4
seed=6886

# Run amen in parallel
library(doParallel) ; library(foreach)
cl=makeCluster(4) ; registerDoParallel(cl)
foreach(ii=1:length(latDims), .packages=c("amen")) %dopar% {
	ameFit = ame_repL(
		Y=yList,Xdyad=xDyadList,Xrow=xNodeList,Xcol=NULL, 
		model="bin", symmetric=TRUE,
		R=latDims[ii], 
		nscan=imps, seed=seed, burn=brn, odens=ods, 
		plot=FALSE, print=FALSE
		) 	
	save(ameFit, 
		file=paste0(resultsPath, 'model_k', latDims[ii],'.rda')
		)
}
stopCluster(cl)