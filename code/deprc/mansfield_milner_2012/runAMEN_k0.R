if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/netsMatter/code/mansfield_milner_2012/setup.R') }

# load amen data
load( paste0(dataPath, 'amenData.rda') )

# params
imps = 50000
brn = 100000
ods = 25
latDims = 0
seed=6886

# Run amen in parallel
ameFit = ame_repL(
	Y=yList,Xdyad=xDyadList,Xrow=xNodeList,Xcol=NULL, 
	model="bin",symmetric=TRUE,intercept=FALSE,R=latDims, 
	nscan=imps, seed=seed, burn=brn, odens=ods, 
	plot=FALSE, print=FALSE, gof=TRUE,
	periodicSave=TRUE, outFile=paste0(resultsPath,'ameFit_k0.rda')
	)
save(ameFit, file=paste0(resultsPath,'ameFit_k0.rda'))