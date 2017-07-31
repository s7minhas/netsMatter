## This script produces AUC and RO plots comparing GLM and AME models
## Script 2  of the model presentation 
##Based on Juan's code for Reiter-Stam

rm(list=ls())

resultsPath = '~/Dropbox/netsMatter/replications/McDonald_2004/data/'



### libraries needed
library(dplyr)
library(magrittr)
library(ggplot2)
library(stringr)
library(gridExtra)
library(Cairo)
library(reshape2)
library(amen)
library(tidyverse)
library(latex2exp)

source('../reiter_stam_2003/helperEx.R')
source('setup.R')

### Load data

## In-sample:

## load( paste0(resultsPath,'ameFit_k0.rda') ) ; ameFit_k0=ameFit
## load( paste0(resultsPath,'ameFit_k1.rda') ) ; ameFit_k1=ameFit
load( paste0(resultsPath,'ameFit_k2.rda') ) ; ameFit_k2=ameFit
##load( paste0(resultsPath,'ameFit_k3.rda') ) ; ameFit_k3=ameFit

##########
## Sender/Receiver effects 
#########

## k = 2
effdat = getAddEffData(fit = ameFit_k2) ##This function is in helperEx.R
effdat$actor = countrycode::countrycode(effdat$actor, 'cown', 'country.name')

addEffPlot(fit = effdat, addEffData = T, row = T)
ggsave(filename = paste0(resultsPath, 'McDonald_sender_k2.pdf'), device = cairo_pdf, width=7, height=7)
addEffPlot(fit = effdat, addEffData = T, row = F)
ggsave(filename = paste0(resultsPath, 'McDonald_receiver_k2.pdf'), device = cairo_pdf, width=7, height=7)


##############
##### AUC and PR

## read data


load(paste0(resultsPath, "McDonald_baseModelGLMObj.rda")) ## mod = GLM object
## some renaming for consistency with other projects
mod_dat=mod$data

dv="cw2mid" ## will adjust the script below to be more general


logitModProbs = 1/(1+exp(-predict(mod))) #numeric vector

logitPred = data.frame(
    actual=mod_dat[,dv],prob=logitModProbs,
  statea = mod_dat$ccode1,
  stateb = mod_dat$ccode2)
logitPred = logitPred[logitPred$statea != logitPred$stateb,]
################################################
################################################

## Amen - K0

## load(paste0(resultsPath, 'ameFit_k0.rda'))
## actual = melt(yList)
## preds = ameFit$'EZ'
## amePred_k0 = melt(preds)
## amePred_k0$prob = 1/(1+exp(-amePred_k0$value))
## amePred_k0$actual = actual$value
## amePred_k0 = na.omit(amePred_k0)

## ## Amen - K1

## load(paste0(resultsPath, 'ameFit_k1.rda'))
## actual = melt(yList)
## preds = ameFit$'EZ'
## amePred_k1 = melt(preds)
## amePred_k1$prob = 1/(1+exp(-amePred_k1$value))
## amePred_k1$actual = actual$value
## amePred_k1 = na.omit(amePred_k1)


## Amen - K2
load(paste0(resultsPath, 'ameFit_k2.rda'))
actual = melt(yList)
preds = ameFit$'EZ'
amePred_k2 = melt(preds)
amePred_k2$prob = 1/(1+exp(-amePred_k2$value))
amePred_k2$actual = actual$value
amePred_k2 = na.omit(amePred_k2)

## ## Amen - K3
## load(paste0(resultsPath, 'ameFit_k3.rda'))
## actual = melt(yList)
## preds = ameFit$'EZ'
## amePred_k3 = melt(preds)
## amePred_k3$prob = 1/(1+exp(-amePred_k3$value))
## amePred_k3$actual = actual$value
## amePred_k3 = na.omit(amePred_k3)

################################
## Performance
###(out of sample) 
################################


##Load out-sample results

load('~/Dropbox/netsMatter/replications/McDonald_2004/data/McDGLMPerf.rda')
load('~/Dropbox/netsMatter/replications/McDonald_2004/data/outsampResults2.rda'); ameOutSamp_k2 <- ameOutSamp_NULL


## ROC plots


rocLogit = 
  roc(prediction = glmOutSamp_wFullSpec$outPerf$pred, 
      actual = glmOutSamp_wFullSpec$outPerf$actual) %>% 
    mutate(model = 'Logit')

rocAme2 = 
  roc(prediction = ameOutSamp_k2$outPerf$pred, 
      actual = ameOutSamp_k2$outPerf$actual) %>% 
  mutate(model = 'AME (K = 2)')


### plotting
pRoc = rbind(rocLogit,rocAme2) 
pRoc$model = as.factor(pRoc$model)
ggCols = brewer.pal(length(levels(pRoc$model)), 'Set1')
rocPlot(pRoc, linetypes = c(1,1,1), legPos = 'top')
ggsave(filename = paste0(resultsPath, 'McDonald_auc_outsamp.pdf'), device = cairo_pdf, width=7, height=7)


## PR

# pr
rocLogit = 
  rocdf(pred = glmOutSamp_wFullSpec$outPerf$pred, 
        obs =  glmOutSamp_wFullSpec$outPerf$actual, type = 'pr') %>% 
  mutate(model = 'Logit')
rocAme2 = 
  rocdf(pred = ameOutSamp_k2$outPerf$pred, 
        obs =  ameOutSamp_k2$outPerf$actual, type = 'pr') %>% 
    mutate(model = 'AME (K = 2)')


# plotting
pRoc = rbind(rocLogit,rocAme2)
pRoc$model = as.factor(pRoc$model)
ggCols = brewer.pal(length(levels(pRoc$model)), 'Set1')
rocPlot(pRoc, linetypes = c(1,1,1), legPos = 'top', type = 'pr')
ggsave(filename = paste0(resultsPath, 'McDonald_pr_outsamp.pdf'), device = cairo_pdf, 
       width=7, height=7)


## AUC ROC
rocLogit = 
  roc(prediction = logitPred$prob, actual = logitPred$actual) %>% 
  mutate(model = 'Logit')

#rocAme0 = roc(prediction = amePred_k0$prob, actual = amePred_k0$actual) %>% 
#  mutate(model = 'AME_K0') 
#rocAme1 = roc(prediction = amePred_k1$prob, actual = amePred_k1$actual) %>% 
#  mutate(model = 'AME_K1')
rocAme2 = roc(prediction = amePred_k2$prob, actual = amePred_k2$actual) %>% 
  mutate(model = 'AME_K2')
##rocAme3 = roc(prediction = amePred_k3$prob, actual = amePred_k3$actual) %>% 
##  mutate(model = 'AME_K3')


pRoc = rbind(rocLogit,
    ## rocAme0, rocAme1,
    rocAme2) #, rocAme3)

pRoc$model = as.factor(pRoc$model)
ggCols = brewer.pal(length(levels(pRoc$model)), 'Set1')
rocPlot(pRoc, linetypes = c(1:5), legPos = 'top')
ggsave(filename = paste0(resultsPath, 'McD_auc.pdf'), device =
       cairo_pdf, width=7, height=7)

# AUC PR
 rocLogit = 
  rocdf(pred = logitPred$prob, obs = logitPred$actual, type = 'pr') %>% 
    mutate(model = 'Logit')

## rocAme0 = 
##   rocdf(pred = amePred_k0$prob, obs = amePred_k0$actual, type = 'pr') %>% 
##   mutate(model = 'AME_K0')
## rocAme1 = 
##   rocdf(pred = amePred_k1$prob, obs = amePred_k1$actual, type = 'pr') %>% 
##   mutate(model = 'AME_K1')

rocAme2 = 
  rocdf(pred = amePred_k2$prob, obs = amePred_k2$actual, type = 'pr') %>% 
  mutate(model = 'AME_K2')

## rocAme3 = 
##   rocdf(pred = amePred_k3$prob, obs = amePred_k3$actual, type = 'pr') %>% 
##   mutate(model = 'AME_K3')

pRoc = rbind(rocLogit, rocAme2)

#    rocAme0, rocAme1, rocAme2, rocAme3)

pRoc$model = as.factor(pRoc$model)
rocPlot(pRoc, linetypes = c(1:5), type = 'pr', legPos = 'top')
ggsave(filename = paste0(resultsPath, 'McD_pr.pdf'), device = cairo_pdf, width=7, height=7)


trim = function (x) { gsub("^\\s+|\\s+$", "", x) } ##should have loaded in setup.R

# get auc summary
aucSumm = do.call('rbind', 
    lapply(predDfs, function(x){ 
        cbind( 'AUC'=getAUC(x$prob, x$actual), 'AUC (PR)'=auc_pr(x$actual, x$prob) ) 
    } ) ) ; rownames(aucSumm) = names(predDfs)
aucSumm = aucSumm[order(aucSumm[,1],decreasing=TRUE),]
aucSumm = trim(format(round(aucSumm, 2), nsmall=2))

### Nodal effects

## load AME node-level data:
load(paste0(resultsPath, 'amenData.rda')) ## xDyadList, xNodeList.R, xNodeList.s, Ylist

## subset data
yList2 = yList[42:47]
yArr = listToArray(actors=sort(unique(unlist(lapply(yList,rownames)))), 
                   Y=yList2, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
yArrSumm = apply(yArr, c(1,2), sum, na.rm=TRUE)
diag(yArrSumm) = 0

library('countrycode')
# fix actor names
rownames(yArrSumm) =
  countrycode::countrycode(rownames(yArrSumm), origin = 'cown', 'country.name', warn = T)
colnames(yArrSumm) =
  countrycode::countrycode(colnames(yArrSumm), origin = 'cown', 'country.name', warn = T)

################
 
uvCols = brewer.pal(11, 'RdBu')[c(11-2, 3)]

dim(yArrSumm)
dim(ameFit$U)
dim(ameFit$V)

circPlot=ggCirc(Y=yArrSumm, U=ameFit$U, V=ameFit$V, vscale=.6, 
  family="Source Sans Pro Light", force=3, 
  lcol='gray85', lsize=.05) +
  scale_color_manual(values=uvCols)

ggsave(circPlot, 
       file=paste0(resultsPath,'McD_circPlot.pdf'), 
       width=12, height=10, device=cairo_pdf)
################


################
# plot vecs on 2d
uDF = data.frame(ameFit$U) ; uDF$name = rownames(uDF) ; uDF$type='Sender Factor Space'
vDF = data.frame(ameFit$V) ; vDF$name = rownames(vDF) ; vDF$type='Receiver Factor Space'
uvDF = rbind(uDF, vDF) ; uvDF$type = factor(uvDF$type, levels=unique(uvDF$type))
uvDF$name = countrycode::countrycode(uvDF$name, 'cown', 'country.name', warn = T)
ggplot(uvDF, aes(x=X1, y=X2, color=type, label=name)) + 
  geom_vline(xintercept = 0, linetype='dashed', color='grey50') + 
  geom_hline(yintercept = 0, linetype='dashed', color='grey50') + 
  scale_color_manual(values=rev(uvCols)) + 
  geom_point() + 
  geom_text_repel() + 
  facet_wrap(~type) + 
  xlab('') + ylab('') + 
  labs(color='') + 
  theme(
    legend.position = 'none',
    axis.ticks=element_blank(),
    axis.text=element_blank(),
    panel.border=element_blank()
  )

ggsave( 
       file=paste0(resultsPath,'McD_2dPlot.pdf'), 
       width=12, height=10, device=cairo_pdf)

print("ROC and Circle Plots Made")
