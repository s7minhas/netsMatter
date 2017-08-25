## This script produces outsample AUC and ROC plots comparing GLM and AME models
## Script 2  of the model presentation; this one only presents k=2
##Based on Juan's code for Reiter-Stam

rm(list=ls())

### libraries needed

if(Sys.info()['user']=='s7m'){
  source('~/Research/netsMatter/code/weeks_2012/setup.R')
  source('~/Research/netsMatter/code/reiter_stam_2003/helperEx.R')
  resultsPath = '~/Dropbox/Research/netsMatter/replications/Weeks2012/replication/output/'
} else {
  source('setup.R')
  source('../reiter_stam_2003/helperEx.R')
  resultsPath = '~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'
}

toLoad <- c("RColorBrewer","dplyr" ,"magrittr" ,"ggplot2" ,"stringr"
            ,"gridExtra","Cairo","reshape2","tidyverse","latex2exp")

loadPkg(toLoad)

##############
##### AUC and PR
## Performance
###(out of sample) 
################################


##Load out-sample results
## warning: this will take several minutes to load:
##(The GLM out samp file is 3.2G!)

load(paste0(resultsPath,'weeksOutPerf.rda'))
load(paste0(resultsPath, 'outsampResults2.rda'))
ameOutSamp_k2 <- ameOutSamp_NULL

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
rocPlot(pRoc, linetypes = c(1,1), legPos = 'top')
ggsave(filename = paste0(resultsPath, 'Weeks_auc_outsamp_k2.pdf'), device = cairo_pdf, width=7, height=7)


## PR

## pr
rocLogit = 
  rocdf(pred = glmOutSamp_wFullSpec$outPerf$pred, 
        obs =  glmOutSamp_wFullSpec$outPerf$actual, type = 'pr') %>% 
  mutate(model = 'Logit')
rocAme2 = 
  rocdf(pred = ameOutSamp_k2$outPerf$pred, 
        obs =  ameOutSamp_k2$outPerf$actual, type = 'pr') %>% 
    mutate(model = 'AME (K = 2)')


## plotting
pRoc = rbind(rocLogit,rocAme2)
pRoc$model = as.factor(pRoc$model)
ggCols = brewer.pal(length(levels(pRoc$model)), 'Set1')
rocPlot(pRoc, linetypes = c(1,1), legPos = 'top', type = 'pr')
ggsave(filename = paste0(resultsPath, 'Weeks_pr_outsamp_k2.pdf'), device = cairo_pdf, 
       width=7, height=7)

####################
### Nodal effects
####################
## load AME node-level data:
load(paste0(resultsPath, 'WeeksamenData.rda')) ## xDyadList, xNodeList.R, xNodeList.s, Ylist

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
       file=paste0(resultsPath,'Weeks_circPlot.pdf'), 
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
       file=paste0(resultsPath,'Weeks_2dPlot.pdf'), 
       width=12, height=10, device=cairo_pdf)

print("ROC and Circle Plots Made")
