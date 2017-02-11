## script to continue AMEN runs from place the last run stopped at

## Load paths and libaries:
rm(list=ls())

## case 1: Dave's server:
if(Sys.info()['user']== 'margaret'){
    source('~/projects/netsmatter/code/netsMatter/code/weeks_2012/setup.R')
}

## One of my machines
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
    source('~/Dropbox/netsMatter/replications/Weeks2012/setup.R')
}

## load amen data

load(paste0(dataPath, 'WeeksamenData.rda') )

## running in parallel varying k
## read in config setting:

source("config.R")

ods = 25
seed=6886
prevModelFiles = paste0(dPath, 'ameFit_k', latDims,'.rda')


## load previous model run

prevModelFiles

load(prevModelFiles)

attributes(fit)

## extract start values
startVals2 = fit$"startVals"
## dump rest
rm(fit)

## 1/30: error is
## Starting burn-in period...
## Error in chol.default(S0) :
##  the leading minor of order 1 is not positive definite

rZ_bin_fc2 <- dget("rZBinfc.R")

assignInNamespace("rZ_bin_fc", rZ_bin_fc2, pos="package:amen")


ameFit = ame_repL(
    Y=yList,
    Xdyad=xDyadList,
    Xrow=xNodeList.s,
    Xcol=xNodeList.r,
    model="bin",
    symmetric=FALSE,
    R=latDims[2],
    nscan=imps, seed=seed, burn=brn, odens=ods,
    plot=FALSE, print=FALSE, startVals=startVals2
)

save(ameFit,
     file=paste0(resultsPath, 'model_k',
                 latDims[ii],as.character(Sys.Date()),
                 '_v2.rda'))
#}

head(startVals2)

    
stopCluster(cl)
