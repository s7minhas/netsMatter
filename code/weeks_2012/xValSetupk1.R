## This script to load objects needed for the Weeks cross-validation

## For three lat dims

#rm(list=ls())

### loading libraries:

library(reshape2)
library(ROCR)
library(caTools)
library(RColorBrewer)
library(devtools) ; devtools::install_github('s7minhas/amen') ; library(amen)

char <- function(x){ as.character(x) }
num <- function(x){ as.numeric(char(x)) }
################

################
## define paths


path = '~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'

## load helpers
source('binPerf.R') #should be in same directory

## load data
load(paste0(dataPath, 'WeeksamenData.rda'))

## load model results
load(paste0(dpath, 'model_k12017-03-15_v2.rda'))
################

## parameters for the cross-val
################
xDyadL = xDyadList
xRowL = xNodeList.s
xColL = xNodeList.r
startVals = ameFit$startVals
seeds=6886
folds=3
R=1
model='bin'
burn=10000
nscan=2000
odens=25
intercept=TRUE
rvar=TRUE
cvar=TRUE
symmetric=FALSE
seed=6886
