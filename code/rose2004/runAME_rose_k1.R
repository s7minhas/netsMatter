## run ame model
#library(devtools)
#devtools::install_github('s7minhas/amen')
library(amen)

# load data
dataPath = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/"
load( paste0(dataPath, 'amenData_rose.rda') )

resultsPath = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/outputData/"

str(yList)
yList[1]
# params

#imps = 50000
#brn = 100000
brn=100; imps=50; ods=10

imps = 3000
brn = 1000
ods = 25

latDims = 1 ## change k here
seed=6886

# Run amen in parallel

ameFit = ame_repL(
	Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
	model="nrm",symmetric=FALSE, # was directed data
	intercept=FALSE,R=latDims,
	nscan=imps, seed=seed, burn=brn, odens=ods,
	plot=FALSE, print=FALSE, gof=TRUE,
	periodicSave=TRUE, outFile=paste0(resultsPath,'ameFit_k1.rda')
	)

save(ameFit, file=paste0(resultsPath,'ameFit_k1.rda'))
