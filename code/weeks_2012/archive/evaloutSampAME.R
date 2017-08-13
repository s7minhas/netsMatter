## Script to look at the AME xval output

## Point to data:
dataPath <-'~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'
## Load data:

##GLM
##load(paste0(dataPath, "weeksOutPerf.rda")) # this has crashed three times 

load(paste0(dataPath, "smallGLMoutSamp.rda"))

attributes(smallGLMoutSamp)

smallGLMoutSamp$GLMaucROC
smallGLMoutSamp$GLMaucPR

## K=0
   
load(paste0(dataPath,"outsampResults0.rda"))

ls()

outFitK0 <- ameOutSamp_NULL

attributes(outFitK0)
outFitK0$aucPR

##K=1
load(paste0(dataPath, "outsampResults1.rda"))

outFitK1 <- ameOutSamp_NULL

outFitK1$aucPR

##K=2

## NEEDS TO RERUN


## K=3

load(paste0(dataPath, "outsampResults3.rda"))
outFitK3 <- ameOutSamp_NULL

round(outFitK3$aucROC, 3)
round(outFitK3$aucPR, 3)

round(outFitK1$aucROC, 3)
round(outFitK1$aucPR, 3)

round(outFitK0$aucROC, 3)
round(outFitK0$aucPR, 3)
