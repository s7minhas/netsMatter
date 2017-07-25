
## Load the code that will run outSamp

source('xValPerformancePlots.R')

## Load the data for this iteration:

load(paste0(path, "fitCrossVal_k1.r"))

## load some helper functions and  parameters                       
source('xValSetupK1.R')

## Script that will generate the data:

## Runing on K=1
ameOutSamp_k1 = ameOutSamp(
    yList=yList, xDyadL=xDyadList,
    xRowL=xRowL, xColL=xColL,
    startVals=ameFit_k1$'startVals', folds = folds, 
    burn=burn, nscan=nscan, odens=odens)

# save
save(
  ameOutSamp_k1,
  file=paste0(path, 'ameCrossValResultsk1.rda')
    )
