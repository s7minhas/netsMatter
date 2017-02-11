
## script to continue AMEN runs from place the last run stopped at
## runs in parallel across values of K.
## pulls config.txt to specify latDims, imps, and burn.

## Load paths and libaries:

## observe that if you run via sudo (such as if you are having to install packages)
## then the Sys.info['user'] is 'root'

#print(Sys.info()['user'])

## case 1: Dave's server:
if(Sys.info()['user']== 'margaret'| Sys.info()['user']== 'root'){
    print("On Dave's server")
    source('setup.R')
    ls()
}

## One of my machines
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
    source('~/Dropbox/netsMatter/replications/Weeks2012/setup.R')
}


## load amen data
load(paste0(dataPath, 'WeeksamenData.rda') )

## read in config setting:
source("config.R")

ods = 25
seed=6886 

## load output from previous models:

prevModelFiles = paste0(dPath, 'ameFit_k', latDims,'.rda')

#if(length(latDims)==2){print("true")}

## Call libraries so that AMEN can run in parallel


library(doParallel) ; library(foreach)
## moved to 2 clusters because 4 made it cry
cl=makeCluster(2) ; registerDoParallel(cl)

ls()

## break into cases:
## if more than one dimension in latDims, indicates that
## you want to run in doParallel:

print(paste0("length of lat dim object is ", length(latDims)))
print(paste0("value of latdim is ", latDims))

if(length(latDims)!= 1){
    print("using do paralell")
    
    print("HAVE YOU FIXED THE prevModFiles code yet?")
    
    foreach(ii=1:length(latDims), .packages=c("amen")) %dopar% {
        
        ## overwrite one of the initiation parameters
        ## specific to the Weeks data
        
        rZ_bin_fc2 <- dget("rZBinfc.R")
        assignInNamespace("rZ_bin_fc", rZ_bin_fc2, pos="package:amen")

        ## FIX ME: need to go back to previous code
        ## to make "prevModelFiles" a list:
        ## load previous model run
        load(prevModelFiles[ii])
        ## extract start vals
        startVals0 = fit$'startVals'
        
        ## dump rest
        rm(fit)
        
        ameFit = ame_repL(
            Y=yList,
            Xdyad=xDyadList,
            Xrow=xNodeList.s,
            Xcol=xNodeList.r,
            model="bin",
            symmetric=FALSE,
            R=latDims[ii],
            nscan=imps, seed=seed, burn=brn, odens=ods,
            plot=FALSE, print=FALSE, startVals=startVals0
        )
        save(ameFit,
             file=paste0(resultsPath, 'model_k',
                         latDims[ii],as.character(Sys.Date()),
                         '_v2.rda'))
    }
    
    stopCluster(cl)

}

## if only one
if(length(latDims)==1){

    print(paste0("Single latent dimension, which is ", latDims))

    rZ_bin_fc2 <- dget("rZBinfc.R")

    assignInNamespace("rZ_bin_fc", rZ_bin_fc2, pos="package:amen")

    ## load previous model run
    load(prevModelFiles)

    ## extract start vals
    startVals0 = fit$'startVals'

    ## dump rest
    rm(fit)

    ## run AME
    ameFit = ame_repL(
        Y=yList, Xdyad=xDyadList,
        Xrow=xNodeList.s, Xcol=xNodeList.r,
        model="bin", symmetric=FALSE,
        R=latDims,  nscan=imps, seed=seed, burn=brn, odens=ods,
        plot=FALSE, print=FALSE, startVals=startVals0
    )
    save(ameFit,file=paste0(resultsPath, 'model_k', latDims[ii],as.character(Sys.Date()),
                     '_v2.rda'))
}

    
