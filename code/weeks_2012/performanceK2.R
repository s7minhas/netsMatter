
## Load the data for this iteration:
##load(paste0(path, ))

source('xValPerformancePlots.R')
                                                         
load(paste0(path, "fitCrossVal_k2.r"))

## load some helper functions and                                                                                                
## parameters
source('xValSetupK2.R')

#################
## Script that generates the comparison data
#################


ameOutSamp_k2 = ameOutSamp(
    yList=yList, xDyadL=xDyadList,
    xRowL=xRowL, xColL=xColL,
  startVals=ameFit_k2$'startVals', folds = folds, 
    burn=burn, nscan=nscan, odens=odens)

# save
save(
    ameOutSamp_k2,
    file=paste0(path, 'ameCrossValResultsk2.rda')
    )
