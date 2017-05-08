
## This script to load objects needed for the Weeks cross-validation

#rm(list=ls())

### loading libraries:

library(reshape2)
library(ROCR)
library(caTools)
library(RColorBrewer)
library(devtools) ; devtools::install_github('s7minhas/amen') ; library(amen)

source('binPerf.R') #should be in same directory
char <- function(x){ as.character(x) }
num <- function(x){ as.numeric(char(x)) }
################

################
# define paths
path = '~/Dropbox/Research/netsMatter/replications/Weeks2012/replication/output/'

# load data
load(paste0(path, 'WeeksamenData.rda'))

# load model results
load(paste0(path, 'model_k22017-04-04_v2.rda'))
################

## parameters for the cross-val
################
xDyadL = xDyadList
xRowL = xNodeList.s
xColL = xNodeList.r
startVals = ameFit$startVals
seeds=6886
folds=3
R=2
model='bin'
burn=10
nscan=50
odens=1
intercept=TRUE
rvar=TRUE
cvar=TRUE
symmetric=FALSE
seed=6886
