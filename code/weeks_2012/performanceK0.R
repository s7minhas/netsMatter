
## Load the data for this iteration:
##load(paste0(path, ))

source('xValPerformancePlots.R')
                                                         
load(paste0(path, "fitCrossVal_k0.r"))

## load some helper functions and                                                                                                
## parameters
source('xValSetupK0.R')

#################
## Script that generates the comparison data
#################

ls()

ameOutSamp_k0 = ameOutSamp(
    yList=yList, xDyadL=xDyadList,
    xRowL=xRowL, xColL=xColL,
  startVals=ameFit_k0$'startVals', folds = folds, 
    burn=burn, nscan=nscan, odens=odens)

# save
save(
    ameOutSamp_k0,
    file=paste0(path, 'ameCrossValResultsk0.rda')
    )

## Investigate the output:

## attributes(ameOutSamp_k0)

## ## for each of the various K=n
## load(paste0(path, "ameCrossValResultsk1.rda" ))

## load(paste0(path, "ameCrossValResultsk2.rda"))

## load(paste0(path, "ameCrossValResultsk3.rda"))

## ameOutSamp_k0$aucROC
## ameOutSamp_k1$aucROC
## ameOutSamp_k2$aucROC
## ameOutSamp_k3$aucROC

## ameOutSamp_k0$aucPR
## ameOutSamp_k1$aucPR
## ameOutSamp_k2$aucPR
## ameOutSamp_k3$aucPR

