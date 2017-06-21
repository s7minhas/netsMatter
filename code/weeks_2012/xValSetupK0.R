## This script to load objects needed for the Weeks cross-validation

#rm(list=ls())

if(Sys.info()['user']== 'margaret'  | Sys.info()['user']== 'root'){
    print("Loading local clone of AMEN")
    library(devtools)
    document('~/projects/netsmatter/amen/')
    install('~/projects/netsmatter/amen/')
}

## if on one of my macs
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
    print("loading AMEN from Github")
    devtools::install <- github('s7minhas/amen') ; library(amen)
}

## Set a theme for gg:
theme <- set(theme <- bw())

## misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

### loading libraries:

library(reshape2)
library(ROCR)
library(caTools)
library(RColorBrewer)
library(devtools) ; devtools::install_github('s7minhas/amen') ; library(amen)

char <- function(x){ as.character(x) }
num <- function(x){ as.numeric(char(x)) }
################

### paths, depending on computer
## if on Dave's server

if(Sys.info()['user']== 'margaret' | Sys.info()['user']== 'root' ){
    dataPath='~/projects/netsmatter/data/'
    dPath='./results/'
    gPath='~/projects/netsmatter/code/netsMatter/code/weeks_2012/'                                          #graphicPath='/results/' # path to dir where i will store any graphics
                                        #resultsPath='/results/' # path to dir where i will store results
    funcPath=paste0(gPath, 'code/helpers/') # helpers directory in ~/Research
}



## If on one of my machines
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
    dataPath='~/Dropbox/netsMatter/replications/Weeks2012'## where the Weeks data lives
    dPath='~/Dropbox/netsMatter/' # general dropox path
    gPath='~/Research/netsMatter/' # path to github in case i need to call in helper functions
    graphicsPath=paste0(dPath, 'replications/Weeks2012/graphics/') # path to dir where i will store any graphics
    resultsPath=paste0(dPath, 'replications/Weeks2012/replication/output/') # path to dir where i will store results
    funcPath=paste0(gPath, 'code/helpers/') # helpers directory in ~/Research

}


## load helpers
source('binPerf.R') #should be in same directory

## load data
load(paste0(dataPath, 'WeeksamenData.rda'))

## load model results

load(paste0(dpath, 'model_k02017-03-15_v2.rda'))
################

## parameters for the cross-val
################
xDyadL = xDyadList
xRowL = xNodeList.s
xColL = xNodeList.r
startVals = ameFit$startVals
seeds=6886
folds=3
R=0
model='bin'
burn=10000
nscan=2000
odens=25
intercept=TRUE
rvar=TRUE
cvar=TRUE
symmetric=FALSE
seed=6886
