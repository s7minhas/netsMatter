rm(list=ls())
resultsPath = '/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/'
plotPath = '/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/'
#
library(magrittr)
library(ggplot2)
library(stringr)
library(gridExtra)
library(Cairo)


# load data
load( paste0(resultsPath,'model_k0_v12.rda') ) ; ameFit_k0=ameFit
load( paste0(resultsPath,'model_k1_v12.rda') ) ; ameFit_k1=ameFit
load( paste0(resultsPath,'model_k2_v12.rda') ) ; ameFit_k2=ameFit
load( paste0(resultsPath,'model_k3_v12.rda') ) ; ameFit_k3=ameFit
load( paste0(resultsPath,'reiterStam_baseModel.rda') )


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

# AME K0
ameBETA = cbind(ameFit_k0$BETA, rho = ameFit_k0$VC[,'rho'])
ameBETA = t(apply(ameBETA, 2, summStats))
colnames(ameBETA) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA = data.frame(ameBETA, stringsAsFactors = F)
ameBETA$var = rownames(ameBETA) ; rownames(ameBETA) = NULL
ameBETA$mod = 'AME_K0'
ameBETA$var[ameBETA$var=='intercept'] = '(Intercept)'

# AME K1
ameBETA1 = cbind(ameFit_k1$BETA, rho = ameFit_k1$VC[,'rho'])
ameBETA1 = t(apply(ameBETA1, 2, summStats))
colnames(ameBETA1) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA1 = data.frame(ameBETA1, stringsAsFactors = F)
ameBETA1$var = rownames(ameBETA1) ; rownames(ameBETA1) = NULL
ameBETA1$mod = 'AME_K1'
ameBETA1$var[ameBETA1$var=='intercept'] = '(Intercept)'

# AME K2
ameBETA2 = cbind(ameFit_k2$BETA, rho = ameFit_k2$VC[,'rho'])
ameBETA2 = t(apply(ameBETA2, 2, summStats))
colnames(ameBETA2) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA2 = data.frame(ameBETA2, stringsAsFactors = F)
ameBETA2$var = rownames(ameBETA2) ; rownames(ameBETA2) = NULL
ameBETA2$mod = 'AME_K2'
ameBETA2$var[ameBETA2$var=='intercept'] = '(Intercept)'

# AME K3
ameBETA3 = cbind(ameFit_k3$BETA, rho = ameFit_k3$VC[,'rho'])
ameBETA3 = t(apply(ameBETA3, 2, summStats))
colnames(ameBETA3) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA3 = data.frame(ameBETA3, stringsAsFactors = F)
ameBETA3$var = rownames(ameBETA3) ; rownames(ameBETA3) = NULL
ameBETA3$mod = 'AME_K3'

# combine and clean up
pDat = rbind(glmBETA, ameBETA, ameBETA1, ameBETA2, ameBETA3)
pDat$var = as.character(pDat$var)
pDat$var[pDat$var == '(Intercept)'] = "intercept"
pDat$var = gsub(pattern = '.dyad', replacement = '', x = pDat$var)


# create groups for plotting
vars = unique(pDat$var)
pDat$group = NA

pDat$group[pDat$var %in% vars[1:4]] = 1
pDat$group[pDat$var %in% vars[5:8]] = 2
pDat$group[pDat$var %in% vars[9:12]] = 3
pDat$group[pDat$var %in% vars[13:17]] = 4

# plot function
ggCoef = function(data, group = NULL)
{
  zp1 = ggplot(data[data$group == group, ], aes(color = mod))
  zp1 = zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
  zp1 = zp1 + geom_linerange(aes(x = var, ymin = lo90,
                                 ymax = hi90),
                             lwd = 1, position = position_dodge(width = 1/2))
  zp1 = zp1 + geom_pointrange(aes(x = var, y = mean, ymin = lo95,
                                  ymax = hi95),
                              lwd = 1/2, position = position_dodge(width = 1/2),
                              shape = 21, fill = "WHITE")
  zp1 = zp1 + coord_flip() + labs(x = "", y = 'coefficient estimate', 
                                  color = 'model type')
  zp1 = zp1 + theme_bw()
  return(zp1)
}

# plot

ggCoef(data = pDat, group = 1) ; ggsave(filename = paste0(resultsPath, 'coefs1.pdf'))
ggCoef(data = pDat, group = 2) ; ggsave(filename = paste0(resultsPath, 'coefs2.pdf'))
ggCoef(data = pDat, group = 3) ; ggsave(filename = paste0(resultsPath, 'coefs3.pdf'))
ggCoef(data = pDat, group = 4) ; ggsave(filename = paste0(resultsPath, 'coefs4.pdf'))
