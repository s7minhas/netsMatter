rm(list=ls())
resultsPath = '/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/'
plotPath = '/Users/juanftellez/Dropbox/netsMatter/replications/0_finalRepFigs/'
#
library(magrittr)
library(ggplot2)
library(stringr)
library(gridExtra)
library(Cairo)
library(reshape2)
library(amen)
library(tidyverse)
library(latex2exp)
source('helperEx.R')



# load data
load( paste0(resultsPath,'model_k0_v12.rda') ) ; ameFit_k0=ameFit
load( paste0(resultsPath,'model_k1_v12.rda') ) ; ameFit_k1=ameFit
load( paste0(resultsPath,'model_k2_v12.rda') ) ; ameFit_k2=ameFit
load( paste0(resultsPath,'model_k3_v12.rda') ) ; ameFit_k3=ameFit
load( paste0(resultsPath,'reiterStam_baseModel.rda') )
load(paste0(resultsPath, 'amenData.rda'))




# Coefficient Plot --------------------------------------------------------


# sum stats
summStats = function(x){
  res=c(mu=mean(x),med=median(x),sd=sd(x),quantile(x,probs=c(0.025,0.05,0.95,0.975)))
  round(res, 3)
}

# GLM
glmBETA = data.frame(var = rownames(base_mod1), 
                     mean = base_mod1[ ,1], 
                     sd = base_mod1[ ,2])
rownames(glmBETA) = NULL
glmBETA$lo95 = glmBETA$mean - qnorm(.975)*glmBETA$sd
glmBETA$hi95 = glmBETA$mean + qnorm(.975)*glmBETA$sd
glmBETA$lo90 = glmBETA$mean - qnorm(.95)*glmBETA$sd  
glmBETA$hi90 = glmBETA$mean + qnorm(.95)*glmBETA$sd
glmBETA$mod = 'GLM'
glmBETA$med = glmBETA$mean
glmBETA$var = c('Intercept', 'Pers/Democ Directed Dyad', 
                'Democ/Pers Directed Dyad',
                'Personal', 'Military', 'Single', 'Democracy', 
                'Contiguous', 'Major Power', 'Ally', 'Higher/Lower Power Ratio', 
                'Economically Advanced', 'Years Since Last Dispute', 'Cubic Spline 1', 
                'Cubic Spline 2', 'Cubic Spline 3')

# AME K0
ameBETA = cbind(ameFit_k0$BETA, rho = ameFit_k0$VC[,'rho'])
ameBETA = t(apply(ameBETA, 2, summStats))
colnames(ameBETA) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA = data.frame(ameBETA, stringsAsFactors = F)
ameBETA$var = rownames(ameBETA) ; rownames(ameBETA) = NULL
ameBETA$mod = 'AME_K0'
ameBETA$var = c('Intercept', 'Personal', 'Military', 'Single', 'Democracy', 
                'Contiguous', 'Ally', 'Major Power', 'Higher/Lower Power Ratio', 
                'Economically Advanced', 'Years Since Last Dispute', 'Cubic Spline 1', 
                'Cubic Spline 2', 'Cubic Spline 3', 'Pers/Democ Directed Dyad', 
                'Democ/Pers Directed Dyad', 'Rho')

# AME K1
ameBETA1 = cbind(ameFit_k1$BETA, rho = ameFit_k1$VC[,'rho'])
ameBETA1 = t(apply(ameBETA1, 2, summStats))
colnames(ameBETA1) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA1 = data.frame(ameBETA1, stringsAsFactors = F)
ameBETA1$var = rownames(ameBETA1) ; rownames(ameBETA1) = NULL
ameBETA1$mod = 'AME_K1'
ameBETA1$var = c('Intercept', 'Personal', 'Military', 'Single', 'Democracy', 
                'Contiguous', 'Ally', 'Major Power', 'Higher/Lower Power Ratio', 
                'Economically Advanced', 'Years Since Last Dispute', 'Cubic Spline 1', 
                'Cubic Spline 2', 'Cubic Spline 3', 'Pers/Democ Directed Dyad', 
                'Democ/Pers Directed Dyad', 'Rho')

# AME K2
ameBETA2 = cbind(ameFit_k2$BETA, rho = ameFit_k2$VC[,'rho'])
ameBETA2 = t(apply(ameBETA2, 2, summStats))
colnames(ameBETA2) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA2 = data.frame(ameBETA2, stringsAsFactors = F)
ameBETA2$var = rownames(ameBETA2) ; rownames(ameBETA2) = NULL
ameBETA2$mod = 'AME_K2'
ameBETA2$var = c('Intercept', 'Personal', 'Military', 'Single', 'Democracy', 
                'Contiguous', 'Ally', 'Major Power', 'Higher/Lower Power Ratio', 
                'Economically Advanced', 'Years Since Last Dispute', 'Cubic Spline 1', 
                'Cubic Spline 2', 'Cubic Spline 3', 'Pers/Democ Directed Dyad', 
                'Democ/Pers Directed Dyad', 'Rho')

# AME K3
ameBETA3 = cbind(ameFit_k3$BETA, rho = ameFit_k3$VC[,'rho'])
ameBETA3 = t(apply(ameBETA3, 2, summStats))
colnames(ameBETA3) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA3 = data.frame(ameBETA3, stringsAsFactors = F)
ameBETA3$var = rownames(ameBETA3) ; rownames(ameBETA3) = NULL
ameBETA3$mod = 'AME_K3'
ameBETA3$var = c('Intercept', 'Personal', 'Military', 'Single', 'Democracy', 
                'Contiguous', 'Ally', 'Major Power', 'Higher/Lower Power Ratio', 
                'Economically Advanced', 'Years Since Last Dispute', 'Cubic Spline 1', 
                'Cubic Spline 2', 'Cubic Spline 3', 'Pers/Democ Directed Dyad', 
                'Democ/Pers Directed Dyad', 'Rho')

# combine and clean up
pDat = rbind(glmBETA, ameBETA, ameBETA1, ameBETA2, ameBETA3)


# drop requested vars
pDat = 
  filter(pDat, !var %in% c('Rho', 'Cubic Spline 1', 'Cubic Spline 2', 
                           'Cubic Spline 3')) %>% 
  filter(mod %in% c('AME_K2', 'GLM'))

pDat$mod[pDat$mod == 'AME_K2'] <- 'AME'
pDat$mod[pDat$mod == 'GLM'] <- 'Logit'


# create order for plotting
varOrder = c('Intercept', 'Pers/Democ Directed Dyad', 
             'Democ/Pers Directed Dyad', 'Personal', 'Military', 'Single', 'Democracy', 
             'Contiguous', 'Ally', 'Major Power', 'Higher/Lower Power Ratio', 
             'Economically Advanced', 'Years Since Last Dispute')
pDat$var = factor(pDat$var,levels = varOrder, ordered = F)
pDat$var = factor(pDat$var,levels = rev(levels(pDat$var)))


# plot
ggCoef(data = pDat)
ggsave(filename = paste0(plotPath, 'reiter_coefs_all.pdf'), device = cairo_pdf, width=7, height=7)


# ggCoef(data = pDat, group = 1) ; ggsave(filename = paste0(resultsPath, 'reiter_coefs1.pdf'), device = cairo_pdf, width=7, height=7)
# ggCoef(data = pDat, group = 2) ; ggsave(filename = paste0(resultsPath, 'reiter_coefs2.pdf'), device = cairo_pdf, width=7, height=7)
# ggCoef(data = pDat, group = 3) ; ggsave(filename = paste0(resultsPath, 'reiter_coefs3.pdf'), device = cairo_pdf, width=7, height=7)
# ggCoef(data = pDat, group = 4) ; ggsave(filename = paste0(resultsPath, 'reiter_coefs4.pdf'), device = cairo_pdf, width=7, height=7)



# Random Effects Plots ----------------------------------------------------

# k = 2
effdat = getAddEffData(fit = ameFit_k2)
effdat$actor = countrycode::countrycode(effdat$actor, 'cown', 'country.name')
addEffPlot(fit = effdat, addEffData = T, row = T)
ggsave(filename = paste0(resultsPath, 'reiter_sender_k2.pdf'), device = cairo_pdf, width=7, height=7)
addEffPlot(fit = effdat, addEffData = T, row = F)
ggsave(filename = paste0(resultsPath, 'reiter_receiver_k2.pdf'), device = cairo_pdf, width=7, height=7)
# k = 3
effdat = getAddEffData(fit = ameFit_k3)
effdat$actor = countrycode::countrycode(effdat$actor, 'cown', 'country.name')
addEffPlot(fit = effdat, addEffData = T, row = T)
ggsave(filename = paste0(resultsPath, 'reiter_sender_k3.pdf'), device = cairo_pdf, width=7, height=7)
addEffPlot(fit = effdat, addEffData = T, row = F)
ggsave(filename = paste0(resultsPath, 'reiter_receiver_k3.pdf'), device = cairo_pdf, width=7, height=7)


# Performance (out of sample) ---------------------------------------------
#load cross-val results
load('/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/glmCrossValResults.rda')
load('/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/ameCrossValResults.rda')

# auc
getAUC(prediction = ameOutSamp_k2$outPerf$pred, 
       actual = ameOutSamp_k2$outPerf$actual)
getAUC(prediction = glmOutSamp_wFullSpec$outPerf$pred, 
       actual = glmOutSamp_wFullSpec$outPerf$actual)

# PR
auc_pr(pred = ameOutSamp_k2$outPerf$pred, 
       obs = ameOutSamp_k2$outPerf$actual)
auc_pr(pred = glmOutSamp_wFullSpec$outPerf$pred, 
       obs = glmOutSamp_wFullSpec$outPerf$actual)

# roc
rocLogit = 
  roc(prediction = glmOutSamp_wFullSpec$outPerf$pred, 
      actual = glmOutSamp_wFullSpec$outPerf$actual) %>% 
  mutate(model = 'Logit')
rocAme2 = 
  roc(prediction = ameOutSamp_k2$outPerf$pred, 
      actual = ameOutSamp_k2$outPerf$actual) %>% 
  mutate(model = 'AME')
rocAme3 = 
  roc(prediction = ameOutSamp_k3$outPerf$pred, 
      actual = ameOutSamp_k3$outPerf$actual) %>% 
  mutate(model = 'AME (K = 3)')

# plotting
pRoc = rbind(rocLogit,rocAme2)
pRoc$model = as.factor(pRoc$model)
ggCols = brewer.pal(length(levels(pRoc$model)), 'Set1')
rocPlot(pRoc, linetypes = c(1,1,1), legPos = 'top')
ggsave(filename = paste0(plotPath, 'reiter_auc_outsamp.pdf'), device = cairo_pdf, width=7, height=7)


# pr
rocLogit = 
  rocdf(pred = glmOutSamp_wFullSpec$outPerf$pred, 
        obs =  glmOutSamp_wFullSpec$outPerf$actual, type = 'pr') %>% 
  mutate(model = 'Logit')
rocAme2 = 
  rocdf(pred = ameOutSamp_k2$outPerf$pred, 
        obs =  ameOutSamp_k2$outPerf$actual, type = 'pr') %>% 
  mutate(model = 'AME')
rocAme3 = 
  rocdf(pred = ameOutSamp_k3$outPerf$pred, 
        obs =  ameOutSamp_k3$outPerf$actual, type = 'pr') %>% 
  mutate(model = 'AME (K = 3)')

# plotting
pRoc = rbind(rocLogit,rocAme2)
pRoc$model = as.factor(pRoc$model)
ggCols = brewer.pal(length(levels(pRoc$model)), 'Set1')
rocPlot(pRoc, linetypes = c(1,1), legPos = 'top', type = 'pr')
ggsave(filename = paste0(plotPath, 'reiter_pr_outsamp.pdf'), device = cairo_pdf, 
       width=7, height=7)



# Node Plots --------------------------------------------------------------

# subset data (map too large otherwise)
yList2 = yList[41:47]

# construct adjacency matrix
yArr = amen::listToArray(actors=sort(unique(unlist(lapply(yList,rownames)))), 
                   Y=yList2, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
yArrSumm = apply(yArr, c(1,2), sum, na.rm=TRUE)
diag(yArrSumm) = 0
# fix actor names
rownames(yArrSumm) =
  countrycode::countrycode(rownames(yArrSumm), origin = 'cown', 'country.name', warn = T)
colnames(yArrSumm) =
  countrycode::countrycode(colnames(yArrSumm), origin = 'cown', 'country.name', warn = T)

################
uvCols = brewer.pal(11, 'RdBu')[c(11-2, 3)]
circPlot=ggCirc(
  Y=yArrSumm, U=ameFit$U, V=ameFit$V, vscale=.6, 
  family="Source Sans Pro Light", force=3, 
  lcol='gray85', lsize=.05) +
  scale_color_manual(values=uvCols)
ggsave(circPlot, 
       file=paste0(resultsPath,'reiter_circPlot.pdf'), 
       width=12, height=10, device=cairo_pdf)
################

# with facet plotting
circPlot=ggCirc(
  Y=yArrSumm, U=ameFit$U, V=ameFit$V, force=5,
  family="Source Sans Pro Light", 
  uLabel='Groups with Common Sending Patterns ($u_{i}$)',
  vLabel='Groups with Common Receiving Patterns ($v_{j}$)',
  removeIsolates=FALSE, showActLinks=FALSE) +
  scale_color_manual(values=uvCols) +
  facet_wrap(~eff, 
             ncol=2, 
             labeller=as_labeller(facet_labeller, default = label_parsed) ) +
  theme(
    strip.text.x = element_text(size = 16, color='white',
                                family="Source Sans Pro Semibold", angle=0, hjust=.2),
    strip.background = element_rect(fill = "#525252", color='#525252')
  )
ggsave(circPlot, 
       file=paste0(resultsPath,'reiter_circPlot_facets.pdf'), 
       width=12, height=10, device=cairo_pdf)


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
  file=paste0(resultsPath,'reiter_2dPlot.pdf'), 
  width=12, height=10, device=cairo_pdf)



# Parameter Table and Triad Plot ------------------------------------------
# table
paramTab = rbind(c(apply(ameFit_k0$VC, 2, mean), apply(ameFit_k0$GOF, 2, mean)),
                 c(apply(ameFit_k1$VC, 2, mean), apply(ameFit_k1$GOF, 2, mean)), 
                 c(apply(ameFit_k2$VC, 2, mean), apply(ameFit_k2$GOF, 2, mean)),
                 c(apply(ameFit_k3$VC, 2, mean), apply(ameFit_k3$GOF, 2, mean))) %>% 
  round(3) %>% 
  as.data.frame()
paramTab$Model = c('AME_K0', 'AME_K1', 'AME_K2', 'AME_K3')
stargazer::stargazer(paramTab, summary = F)

# triad plot


# Performance (in-sample) -------------------------------------------------

# read data
resultsPath = '/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/'

# logit model
load( paste0(resultsPath,'reiterStam_baseModel.rda') )
logitModProbs = 1/(1+exp(-predict(mod1)))
logitPred = data.frame(
  actual=mod_dat$sideaa,prob=logitModProbs,
  statea = mod_dat$statea,
  stateb = mod_dat$stateb)
logitPred = logitPred[logitPred$statea != logitPred$stateb,]
################################################

################################################
# Amen - K0
load(paste0(resultsPath, 'model_k0_v12.rda'))
actual = melt(yList)
preds = ameFit$'EZ'
amePred_k0 = melt(preds)
amePred_k0$prob = 1/(1+exp(-amePred_k0$value))
amePred_k0$actual = actual$value
amePred_k0 = na.omit(amePred_k0)
# Amen - K1
load(paste0(resultsPath, 'model_k1_v12.rda'))
actual = melt(yList)
preds = ameFit$'EZ'
amePred_k1 = melt(preds)
amePred_k1$prob = 1/(1+exp(-amePred_k1$value))
amePred_k1$actual = actual$value
amePred_k1 = na.omit(amePred_k1)
# Amen - K2
load(paste0(resultsPath, 'model_k2_v12.rda'))
actual = melt(yList)
preds = ameFit$'EZ'
amePred_k2 = melt(preds)
amePred_k2$prob = 1/(1+exp(-amePred_k2$value))
amePred_k2$actual = actual$value
amePred_k2 = na.omit(amePred_k2)
# Amen - K3
load(paste0(resultsPath, 'model_k3_v12.rda'))
actual = melt(yList)
preds = ameFit$'EZ'
amePred_k3 = melt(preds)
amePred_k3$prob = 1/(1+exp(-amePred_k3$value))
amePred_k3$actual = actual$value
amePred_k3 = na.omit(amePred_k3)
################################

# Organize pred DFs
predDfs = list( Logit=logitPred, AME_k0 = amePred_k0, 
                AME_k1 = amePred_k1, AME_k2 = amePred_k2, 
                AME_k3 = amePred_k3)


# AUC ROC
rocLogit = 
  roc(prediction = logitPred$prob, actual = logitPred$actual) %>% 
  mutate(model = 'Logit')
rocAme0 = roc(prediction = amePred_k0$prob, actual = amePred_k0$actual) %>% 
  mutate(model = 'AME_K0') 
rocAme1 = roc(prediction = amePred_k1$prob, actual = amePred_k1$actual) %>% 
  mutate(model = 'AME_K1')
rocAme2 = roc(prediction = amePred_k2$prob, actual = amePred_k2$actual) %>% 
  mutate(model = 'AME_K2')
rocAme3 = roc(prediction = amePred_k3$prob, actual = amePred_k3$actual) %>% 
  mutate(model = 'AME_K3')

pRoc = rbind(rocLogit, rocAme0, rocAme1, rocAme2, rocAme3)
pRoc$model = as.factor(pRoc$model)
ggCols = brewer.pal(length(levels(pRoc$model)), 'Set1')
rocPlot(pRoc, linetypes = c(1:5), legPos = 'top')
ggsave(filename = paste0(resultsPath, 'reiter_auc-insamp.pdf'), device = cairo_pdf, width=7, height=7)
# AUC PR
rocLogit = 
  rocdf(pred = logitPred$prob, obs = logitPred$actual, type = 'pr') %>% 
  mutate(model = 'Logit')
rocAme0 = 
  rocdf(pred = amePred_k0$prob, obs = amePred_k0$actual, type = 'pr') %>% 
  mutate(model = 'AME_K0')
rocAme1 = 
  rocdf(pred = amePred_k1$prob, obs = amePred_k1$actual, type = 'pr') %>% 
  mutate(model = 'AME_K1')
rocAme2 = 
  rocdf(pred = amePred_k2$prob, obs = amePred_k2$actual, type = 'pr') %>% 
  mutate(model = 'AME_K2')
rocAme3 = 
  rocdf(pred = amePred_k3$prob, obs = amePred_k3$actual, type = 'pr') %>% 
  mutate(model = 'AME_K3')

pRoc = rbind(rocLogit, rocAme0, rocAme1, rocAme2, rocAme3)
pRoc$model = as.factor(pRoc$model)
rocPlot(pRoc, linetypes = c(1:5), type = 'pr', legPos = 'top')
ggsave(filename = paste0(resultsPath, 'reiter_pr-insamp.pdf'), device = cairo_pdf, width=7, height=7)


# get auc summary
aucSumm = do.call('rbind', 
                  lapply(predDfs, function(x){ 
                    cbind( 'AUC'=getAUC(x$prob, x$actual), 'AUC (PR)'=auc_pr(x$actual, x$prob) ) 
                  } ) ) ; rownames(aucSumm) = names(predDfs)
aucSumm = aucSumm[order(aucSumm[,1],decreasing=TRUE),]
aucSumm = trim(format(round(aucSumm, 2), nsmall=2))
