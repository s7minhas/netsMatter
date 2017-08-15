
## Script to replicate the results in McDonald (2004)
#
## If on Dave's computer:
if(Sys.info()['user']== 'margaret'| Sys.info()['user']== 'root'){
    print("On Dave's server")
    source('setup.R')
    ls()
}

## If my own
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
    dPath='~/Dropbox/netsMatter/replications/McDonald_2004/data/'
    
    ## load libraries
### install/load libraries
    
    loadPkg=function(toLoad){
	for(lib in toLoad){
            if(!(lib %in% installed.packages()[,1])){ 
                install.packages(lib, repos='http://cran.rstudio.com/') }
            suppressMessages( library(lib, character.only=TRUE) )
	}
    }
    
    loadPkg(c('amen'))
}

###################################
## load amen data and config
###################################

## data
load(paste0(dPath, 'amenData.rda') )


##params: read in from config.R
## imps, latDims, brn
source("config.R")

## if we're continuing to run:
print(paste0("Single latent dimension, which is ", latDims))

### 7/29 Can't load previous data run without intercepts

## If needed: load previous model run

prevModelFiles = paste0(dPath, 'ameFitIntercepts_k', latDims,'.rda')
load(prevModelFiles)

#head(fit$'BETA')

startVals0=ameFit$'startVals'


################################
## Trial run

## params that are stable

seed=6889


ameFit =  ame_repL(
    Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
    model="bin",symmetric=FALSE, # McDonald model is symmetric, but shouldn't matter
    intercept=TRUE, ## updated 7/29 b/c do want intercepts
    R=latDims,
    nscan=imps, seed=seed, burn=brn, odens=ods,
    plot=FALSE, print=FALSE, gof=TRUE,
    startVals=startVals0,
    periodicSave=TRUE, outFile=paste0(dPath, 'ameFitIntercepts_k', latDims,'.rda')
    )   


save(ameFit, file=paste0(dPath, 'ameFitIntercepts_k', latDims,'.rda'))
