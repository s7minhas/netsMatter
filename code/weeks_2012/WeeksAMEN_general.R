####################################
## Preliminaries
###################################

## MJF: setup.R loads packages and directory  paths
## hardcoded to be in the same directory as this script
## to make it easier to port to different computers-- just keep dir together

source('./setup.R')

ls()

## load amen data
AMENData <- paste0(dataPath, "/replication/output/", 'WeeksamenData.rda')
load(AMENData)

##params: read in from config.R
## imps, latDims, brn
source("config.R")

## print workspace
ls()

##imps = as.numeric(args[2]) 
##brn = as.numeric(args[3] ## burn-in
ods = 10
##latDims = as.numeric(args[1])
seed= 6889

kchar <- as.character(latDims)

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
    outFile=paste0(resultsPath, paste0('ameFit_k', kchar, '.rda')))


## diagnostic plots

pdf(file=paste0(resultsPath, "Betaplot_k", kchar, ".pdf"))
paramPlot(ameFit$BETA[,1:6])
dev.off()

pdf(file=paste0(resultsPath,"VCplot_k",  kchar, ".pdf"))
paramPlot(ameFit$VC)
dev.off()

pdf(paste0(resultsPath, "GOFplot_k", kchar, ".pdf"))
gofPlot(ameFit$GOF, symmetric=FALSE)
dev.off()


#save(ameFit, file=paste0(resultsPath,'ameFit_k0.rda'))


