## Script to look at the AME xval output

dataPath <- "~/Dropbox/netsMatter/replications/McDonald_2004/data/"


## GLM

load(paste0(dataPath, "smallMcDGLMStats.rda"))
GLMoutSamp


ls()
attributes(smallMcDGLMStats)

smallMcDGLMStats$ROC
smallMcDGLMStats$PR

##K0

load("outsampResults0.rda")

outFitK0 <- ameOutSamp_NULL

outFitK0$aucROC
outFitK0$aucPR

## K=1
   
load("outsampResults1.rda")

outFitK1 <- ameOutSamp_NULL

outFitK1$aucROC
outFitK1$aucPR


##K=2
## 2 and 3 are the same....
load("outsampResults2.rda")

outFitK2 <- ameOutSamp_NULL

outFitK2$aucROC
outFitK2$aucPR


##K=3

load("outsampResults3.rda")

outFitK3 <- ameOutSamp_NULL


outFitK3$aucROC
outFitK3$aucPR


### All
ls()


## GLM
smallMcDGLMStats$ROC
smallMcDGLMStats$PR

## AMEN (k=0)
outFitK0$aucROC
outFitK0$aucPR

## AMEN (k=1)
outFitK1$aucROC
outFitK1$aucPR


##AMEN (k=2)
outFitK2$aucROC
outFitK2$aucPR

## AMEN (k=3)
outFitK3$aucROC
outFitK3$aucPR


