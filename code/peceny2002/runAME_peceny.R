## run ame model
#library(devtools)
#devtools::install_github('s7minhas/amen')
library(amen)

# load data
dataPath = "/Users/howardliu/Dropbox/netsMatter/replications/peceny2002/"
load( paste0(dataPath, 'amenData_peceny.rda') )

#resultsPath = "/Users/howardliu/desktop/"

str(yList)
yList[1]
# params

#imps = 50000
#brn = 100000

imps = 3000
brn = 1000
ods = 25
latDims = 0
seed=6886

# Run amen in parallel

ameFit = ame_repL(
	Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
	model="bin",symmetric=TRUE,intercept=FALSE,R=latDims,
	nscan=imps, seed=seed, burn=brn, odens=ods,
	plot=FALSE, print=FALSE, gof=TRUE,
	periodicSave=TRUE, outFile=paste0(resultsPath,'ameFit_k0.rda')
	)
