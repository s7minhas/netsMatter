####################################
## Preliminaries
###################################

## MJF: setup files load packages and provide save paths
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
    source('~/Research/netsMatter/code/weeks_2012/code/setup.R')
}

ls()


## load amen data
AMENData <- paste0(dataPath, "/replication/output/", 'WeeksamenData.rda')
load(AMENData)

# params
imps = 100 
brn = 200 ## burn-in
ods = 10 
latDims = 1
seed= 6889

ameFit = ame_repL(
    Y=yList,
    Xdyad=xDyadList,
    Xrow=xNodeList.s, ##SENDER
    Xcol= xNodeList.r,  ## Reciever
    model="bin",symmetric=FALSE,
    intercept=FALSE,R=latDims, 
    nscan=imps, seed=seed, burn=brn, odens=ods, 
    plot=FALSE, print=FALSE, gof=TRUE,
    periodicSave=TRUE,
    outFile=paste0(resultsPath,'ameFit_k1.rda'))

##save(ameFit, file=paste0(resultsPath,'ameFit_k1.rda'))


