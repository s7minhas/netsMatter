## This script produces coefficient plots for AME output K=0:3 Aand the GLM model
## Script 1 of the model presentation 
##Based on Juan's code for Reiter-Stam and mine for Weeks

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
source('../weeks_2012/helperEx.R')
library(amen)


## Load in the data

load( paste0(resultsPath,'ameFit_k0.rda') ) ; ameFit_k0=ameFit
load( paste0(resultsPath,'ameFit_k1.rda') ) ; ameFit_k1=ameFit
load( paste0(resultsPath,'ameFit_k2.rda') ) ; ameFit_k2=ameFit
load( paste0(resultsPath,'ameFit_k3.rda') ) ; ameFit_k3=ameFit

## GLM-- results of base model

load(paste0(resultsPath,'McDonald_baseModel.rda'));  base_mod1=round(modSumm,3) #this is original coefficient estimates

## Meaningful names on IVs:

dv= 'cw2mid'

ivs=c("Spline0","Spline1","Spline2","Spline3","Shared Alliance","Contiguous","Log Capabilities Ratio","Trade Dependence","Preconflict GDP Change","Lowest Dyadic Polity Score","Capabilities","Logged GDP","Logged Cap. Distance","Major Power In Dyad","Higest Barrier To Trade")


# sum stats
summStats = function(x){
  res=c(mu=mean(x),med=median(x),sd=sd(x),quantile(x,probs=c(0.025,0.05,0.95,0.975)))
  round(res, 3)
}

## Point estimates for the GLM model

glmBETA = data.frame(var = rownames(base_mod1), 
    mean = base_mod1[ ,1], 
    sd = base_mod1[ ,2])


class(base_mod1)
attributes(base_mod1)
head(ameFit_k2$BETA)

colnames(ameFit_k2$BETA)

     
rownames(glmBETA) = NULL
glmBETA$lo95 = glmBETA$mean - qnorm(.975)*glmBETA$sd
glmBETA$hi95 = glmBETA$mean + qnorm(.975)*glmBETA$sd
glmBETA$lo90 = glmBETA$mean - qnorm(.95)*glmBETA$sd  
glmBETA$hi90 = glmBETA$mean + qnorm(.95)*glmBETA$sd
glmBETA$mod = 'GLM'
glmBETA$med = glmBETA$mean
glmBETA$prezvar = c("Intercept", ivs)

attributes(ameFit_k0)

class(ameFit_k0$BETA)

dim(ameFit_k0$BETA) ## still 15, which means no intercept. wth?

head(ameFit_k0$BETA)

## AME IVS

## same as the GLM here

## AME K0

ameBETA = cbind(ameFit_k0$BETA, rho = ameFit_k0$VC[,'rho'])
ameBETA = t(apply(ameBETA, 2, summStats))
colnames(ameBETA) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA = data.frame(ameBETA, stringsAsFactors = F)
ameBETA$var = rownames(ameBETA) ; rownames(ameBETA) = NULL
ameBETA$mod = 'AME_K0'
ameBETA$prezvar = c(ivs, "rho") ## CHECK: WHY IS THERE NO INTERCEPT?

# AME K1
ameBETA1 = cbind(ameFit_k1$BETA, rho = ameFit_k1$VC[,'rho'])
ameBETA1 = t(apply(ameBETA1, 2, summStats))
colnames(ameBETA1) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA1 = data.frame(ameBETA1, stringsAsFactors = F)
ameBETA1$var = rownames(ameBETA1) ; rownames(ameBETA1) = NULL
ameBETA1$mod = 'AME_K1'
ameBETA1$prezvar = c(ivs, "rho")

# AME K2
ameBETA2 = cbind(ameFit_k2$BETA, rho = ameFit_k2$VC[,'rho'])
ameBETA2 = t(apply(ameBETA2, 2, summStats))
colnames(ameBETA2) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA2 = data.frame(ameBETA2, stringsAsFactors = F)
ameBETA2$var = rownames(ameBETA2) ; rownames(ameBETA2) = NULL
ameBETA2$mod = 'AME_K2'
ameBETA2$prezvar =  c(ivs, "rho")

# AME K3
ameBETA3 = cbind(ameFit_k3$BETA, rho = ameFit_k3$VC[,'rho'])
ameBETA3 = t(apply(ameBETA3, 2, summStats))
colnames(ameBETA3) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA3 = data.frame(ameBETA3, stringsAsFactors = F)
ameBETA3$var = rownames(ameBETA3) ; rownames(ameBETA3) = NULL
ameBETA3$mod = 'AME_K3'
ameBETA3$prezvar = c( ivs, "rho")

## combine and clean up
## note that have to remove GLM results because
## the AME results have more parameters
pDat = rbind(glmBETA, ameBETA, ameBETA1, ameBETA2, ameBETA3)


# create groups for plotting
vars = unique(pDat$prezvar)
pDat$group = NA
varOrder = c(ivs, "rho")
pDat$prezvar = factor(pDat$prezvar,levels = varOrder, ordered = F) 
pDat$prezvar = factor(pDat$prezvar,levels =
    rev(levels(pDat$var)))##this is needed b/c of the axis orientation


length(ivs)## 15

pDat$group[pDat$prezvar %in% c("Intercept", ivs[1:5])] = 1
pDat$group[pDat$prezvar %in% ivs[6:10]] = 2
pDat$group[pDat$prezvar %in% c(ivs[11:15], "rho")] = 3
head(pDat)

# plot function for AME models
ggCoef = function(data, group = NULL)
{
  if(!is.null(group))
  {
    zp1 = ggplot(data[data$group == group, ], aes(color = mod))
  } else{
    zp1 = ggplot(data, aes(color = mod))
  }
  zp1 = zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
  zp1 = zp1 + geom_linerange(aes(x = prezvar, ymin = lo90,
                                 ymax = hi90),
                             lwd = 1, position = position_dodge(width = .7))
  zp1 = zp1 + geom_pointrange(aes(x = prezvar, y = mean, ymin = lo95,
                                  ymax = hi95),
                              lwd = 1/2, position = position_dodge(width = .7),
                              shape = 21, fill = "WHITE", fatten=.5)
  zp1 = zp1 + coord_flip() + labs(x = "", y = '', 
                                  color = 'model type')
  zp1 = zp1 + theme_bw() +
    theme(
      legend.position='top', legend.title=element_blank(),
      legend.text=element_text(family="Source Sans Pro Light"),
      panel.border=element_blank(),
      axis.ticks=element_blank(),
      axis.text.x=element_text(family="Source Sans Pro Light"),
      axis.text.y=element_text(family="Source Sans Pro Light", hjust=0)
    )
  return(zp1)
}


### make a version without the trade dyads, for comparison

## All coefficients
ggCoef(data = pDat) ;
ggsave(filename = paste0(resultsPath, 'McDAME_coefs.pdf'), device = cairo_pdf, width=7, height=7)

## ## Just GLM Coefficients
##  ggCoef2(data=glmBETA)
## ggsave(filename = paste0(resultsPath, 'McDGLM_AllCoefs.pdf'), device = cairo_pdf, width=7, height=7)


## A few at a time
ggCoef(data = pDat, group = 1) ; ggsave(filename = paste0(resultsPath, 'McD_coefs1.pdf'), device = cairo_pdf, width=7, height=7)
ggCoef(data = pDat, group = 2) ; ggsave(filename = paste0(resultsPath, 'McD_coefs2.pdf'), device = cairo_pdf, width=7, height=7)
ggCoef(data = pDat, group = 3) ; ggsave(filename = paste0(resultsPath, 'McD_coefs3.pdf'), device = cairo_pdf, width=7, height=7)

## Conclude
print("End of Coefficient Plot Creation-- check output directory")
