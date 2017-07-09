## Script to look at the AME xval output


## Load data:

load("outsampResults3.rda")

ls()

attributes(ameOutSamp_NULL)

## renaming to ease typing
outFitK3 <- ameOutSamp_NULL

outFitK3$aucROC

outFitK3$aucPR

outFitK3$aucByFold

## K=0
   
load("outsampResults0.rda")

ls()

outFitK0 <- ameOutSamp_NULL

outFitK0$aucPR

##K=1
load("outsampResults1.rda")

outFitK1 <- ameOutSamp_NUL
outFitK1$aucPR

##K=2

outFitK0$aucPR
outFitK1$aucPR
outFitK3$aucPR
