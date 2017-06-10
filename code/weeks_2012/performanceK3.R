
## Load the data for this iteration:
##load(paste0(path, ))

source('xValPerformancePlots.R')
                                                         
load(paste0(path, "fitCrossVal_k3.r"))

## load some helper functions and                                                                                                
## parameters
source('xValSetupK3.R')

#################
## Script that generates the comparison data
#################


ameOutSamp_k3 = ameOutSamp(
    yList=yList, xDyadL=xDyadList,
    xRowL=xRowL, xColL=xColL,
  startVals=ameFit_k3$'startVals', folds = folds, 
    burn=burn, nscan=nscan, odens=odens)

# save
save(
    ameOutSamp_k3,
    file=paste0(path, 'ameCrossValResultsk3.rda')
    )
