## script to continue AMEN runs from place the last run stopped at

## Load paths and libaries:
rm(list=ls())

## Where am I:
## case 1: Dave's server:
if(Sys.info()['user']== 'margaret' |(Sys.info()['user']== 'root')){
    source('setup.R') 
}

## One of my machines:
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
    source('~/Dropbox/netsMatter/replications/Weeks2012/setup.R')
}

## load amen data
ls()

load(paste0(dataPath, 'WeeksamenData.rda') )

## running in parallel varying k
## read in config setting:

source("config.R")

ods = 25
seed=6886

##prevModelFiles = paste0(dPath, 'ameFit_k', latDims,'.rda')
#### load previous model run

##load(prevModelFiles)

#### extract start values
##startVals2 = fit$"startVals"
#### dump rest
##rm(fit)


## Code to patch the problem that results in "chol.default(S0):

rZ_bin_fc2 <- dget("rZBinfc.R")
assignInNamespace("rZ_bin_fc", rZ_bin_fc2, pos="package:amen")


## Call ame_repL
ameFit = ame_repL(
    Y=yList,
    Xdyad=xDyadList,
    Xrow=xNodeList.s,
    Xcol=xNodeList.r,
    model="bin",
    symmetric=FALSE,
    R=latDims,
    nscan=imps,
    seed=seed, burn=brn, odens=ods,
    plot=FALSE,
    print=FALSE,
    ##startVals=startVals2
)

save(ameFit,
     file=paste0(dataPath, 'model_k',
                 as.character(latDims),"_",as.character(Sys.Date()),'.rda'))

