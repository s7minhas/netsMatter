## This script to load objects and paths  needed for the Weeks cross-validation
################

### paths, depending on computer
## if on Dave's server

if(Sys.info()['user']== 'margaret' | Sys.info()['user']== 'root' ){
    dataPath='~/projects/netsmatter/data/'
    dPath='./results/'
    gPath='~/projects/netsmatter/code/netsMatter/code/McDonald2004/'
    ##graphicPath='/results/' # path to dir where i will store any graphics
    ##resultsPath='/results/' # path to dir where i will store results
    funcPath=paste0(gPath, 'code/helpers/') # helpers directory in ~/Research
}



## If on one of my machines
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
    dataPath='~/Dropbox/netsMatter/replications/McDonald_2004/data/'## where the McD Data lives
    dPath=dataPath ##ugly hack, but want the actual code to be independent of computer
    gPath='~/Research/netsMatter/' # path to github in case i need to call in helper functions
    funcPath=paste0(gPath, 'code/helpers/') # helpers directory in ~/Research
}


## load helpers
source('../weeks_2012/binPerf.R') #some helpers, live in the weeks directory

## load data
load(paste0(dataPath, 'amenData.rda'))

## load model results
load(paste0(dPath, 'ameFit_k1.rda'))

## changing some names for consistency
fit=ameFit
################

## parameters for the cross-val
################
## xDyadL = 
## xRowL = 
## xColL = 
startVals = ameFit$startVals
rseed=6886
folds=30
latDims=1
model='bin'
burnin=1000
nscans=20000
ods=25
intercept=TRUE
rvar=TRUE
cvar=TRUE
symmetric=FALSE
