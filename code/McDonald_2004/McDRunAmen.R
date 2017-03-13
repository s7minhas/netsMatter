
## Script to replicate the results in McDonald (2004)

## paths
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
    dPath='~/Dropbox/netsMatter/replications/McDonald_2004/data/'
}


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


###################################
## load amen data and config
###################################

## data
load(paste0(dPath, 'amenData.rda') )


##params: read in from config.R
## imps, latDims, brn
source("config.R")

################################
## Trial run
### (using Howard's code)

## params that are stable

seed=6889
ods=10
#### k =0 #####

# Run amen in parallel

ameFit = ame_repL(
    Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
    model="bin",symmetric=FALSE, # was directed data
    intercept=FALSE,R=latDims,
    nscan=imps, seed=seed, burn=brn, odens=ods,
    plot=FALSE, print=FALSE, gof=TRUE,
    periodicSave=TRUE, outFile=paste0(dPath,'ameFit_k0.rda')
    )

save(ameFit, file=paste0(dPath,'ameFit_k0.rda'))
