## Script to look at the AME xval output

## Load data:

##K0

load("outsampResults0.rda")

outFitK0 <- ameOutSamp_NULL

outFitK0$aucROC
outFitK0$aucPR

## K=1
   
load("outsampResults1.rda")

outFitK1 <- ameOutSamp_NULL

outFitK1$aucPR

##K=2
## 2 and 3 are the same....
load("outsampResults2.rda")

outFitK2 <- ameOutSamp_NULL
outFitK2$aucPR

outFitK2$aucByFold
##K=3

load("outsampResults3.rda")
outFitK3 <- ameOutSamp_NULL


outFitK3$aucROC

outFitK3$aucPR

outFitK3$aucByFold

### All

outFitK0$aucPR
outFitK1$aucPR
outFitK2$aucPR
outFitK3$aucPR
