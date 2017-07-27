## This script produces coefficient plots for AME output K=0:3 and the GLM model
## Script 1 of the model presentation 
##Based on Juan's code for Reiter-Stam

rm(list=ls())

resultsPath = '~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'
inputPath= '~/Dropbox/netsMatter/replications/Weeks2012/replication/input/'
plotPath = '~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'

### libraries needed
library(dplyr)
library(magrittr)
library(ggplot2)
library(stringr)
library(gridExtra)
library(Cairo)
library(reshape2)
source('helperEx.R')
library(amen)



# load data
load( paste0(resultsPath,'model_k02017-03-15_v2.rda') ) ; ameFit_k0=ameFit
load( paste0(resultsPath,'model_k12017-03-15_v2.rda') ) ; ameFit_k1=ameFit
load( paste0(resultsPath,'model_k22017-04-04_v2.rda') ) ; ameFit_k2=ameFit
load( paste0(resultsPath,'model_k32017-03-14_v2.rda') ) ; ameFit_k3=ameFit

load(paste0(inputPath,'weeks_baseModel.rda')); base_mod1=round(modSumm,3) #this is original coefficient estimates

## Meaningful names on IVs:

ivs <- c("Machine", "Junta", "Boss", "Strongman",
         "Other Type","New/Unstable Regime", "Reciever Dem", "Military Capabilities Side1",
         "Military Capabilities Side2", "Initator Share of Capabilities",
         "Low Trade Dependence", "Both Major Powers",
         "Initiator Minor Power",
         "Initiator Major Power", "Contiguous", "Log Dist. Between Capitals",
         "AllianceSimilarity",
         "Side1 Alliance Simlarity Leader",
         "Side2 Alliance Similarity Leader",
         "TimeSinceLastConflict", "Spline1", "Spline2", "Spline3")
         

# sum stats
summStats = function(x){
  res=c(mu=mean(x),med=median(x),sd=sd(x),quantile(x,probs=c(0.025,0.05,0.95,0.975)))
  round(res, 3)
}

## Point estimates for the GLM model

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
glmBETA$var = c("Intercept", ivs)

glmBETA
base_mod1

## AME IVS

## What happend to:
## dem2, cap2, side 1 and side 2 similarity with leader

ivsAME <- c("Machine_sender", "Junta_sender", "Boss_sender", "Strongman_sender",
         "Other Type_sender","New/Unstable Regime_sender",
            "Military Capabilities Side 1_sender","System Leader Side 1_sender",
            "Machine_reciever",
            "Junta_reciever", "Boss_reciever", "Strongman_reciever", 
            "Other Type_reciever","New/Unstable Regime_reciever",
            "Military Capabilities Side 1_reciever",
            "System Leader Side 1_reciever",
            "Low Trade Dependence_dyad", "Both Major Powers_dyad",
            "Minor/Major_dyad", "Major/Minor_dyad", "Contiguous_dyad",
            "Log Dist_dyad", "AllianceSimilarity_dyad",
            "TimeSinceLastConflict_dyad", "Spline1_dyad", "Spline2_dyad", "Spline3_dyad")

## AME K0
ameBETA = cbind(ameFit_k0$BETA, rho = ameFit_k0$VC[,'rho'])
ameBETA = t(apply(ameBETA, 2, summStats))
colnames(ameBETA) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA = data.frame(ameBETA, stringsAsFactors = F)
ameBETA$var = rownames(ameBETA) ; rownames(ameBETA) = NULL
ameBETA$mod = 'AME_K0'
ameBETA$prezvar = c('Intercept',ivsAME, "rho")

# AME K1
ameBETA1 = cbind(ameFit_k1$BETA, rho = ameFit_k1$VC[,'rho'])
ameBETA1 = t(apply(ameBETA1, 2, summStats))
colnames(ameBETA1) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA1 = data.frame(ameBETA1, stringsAsFactors = F)
ameBETA1$var = rownames(ameBETA1) ; rownames(ameBETA1) = NULL
ameBETA1$mod = 'AME_K1'
ameBETA1$prezvar = c("Intercept", ivsAME, "rho")

# AME K2
ameBETA2 = cbind(ameFit_k2$BETA, rho = ameFit_k2$VC[,'rho'])
ameBETA2 = t(apply(ameBETA2, 2, summStats))
colnames(ameBETA2) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA2 = data.frame(ameBETA2, stringsAsFactors = F)
ameBETA2$var = rownames(ameBETA2) ; rownames(ameBETA2) = NULL
ameBETA2$mod = 'AME_K2'
ameBETA2$prezvar =  c("Intercept", ivsAME, "rho")

# AME K3
ameBETA3 = cbind(ameFit_k3$BETA, rho = ameFit_k3$VC[,'rho'])
ameBETA3 = t(apply(ameBETA3, 2, summStats))
colnames(ameBETA3) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA3 = data.frame(ameBETA3, stringsAsFactors = F)
ameBETA3$var = rownames(ameBETA3) ; rownames(ameBETA3) = NULL
ameBETA3$mod = 'AME_K3'
ameBETA3$prezvar = c("Intercept", ivsAME, "rho")

## combine and clean up
## note that have to remove GLM results because
## the AME results have more parameters
pDat = rbind(ameBETA, ameBETA1, ameBETA2, ameBETA3)


# create groups for plotting
vars = unique(pDat$prezvar)
pDat$group = NA
## varOrder = c('Intercept', 'Pers/Democ Directed Dyad', 
##             'Democ/Pers Directed Dyad', 'Personal', 'Military', 'Single', 'Democracy', 
##             'Contiguous', 'Ally', 'Major Power', 'Higher/Lower Power Ratio', 
##             'Economically Advanced', 'Years Since Last Dispute', 'Cubic Spline 1', 
##             'Cubic Spline 2', 'Cubic Spline 3', 'Rho')
##pDat$var = factor(pDat$var,levels = varOrder, ordered = F)
##pDat$var = factor(pDat$var,levels = rev(levels(pDat$var)))


length(unique(pDat$var))

ls()

pDat$group[pDat$prezvar %in% c("Intercept", ivsAME[1:5])] = 1
pDat$group[pDat$prezvar %in% ivsAME[6:10]] = 2
pDat$group[pDat$prezvar %in% ivsAME[11:15]] = 3
pDat$group[pDat$prezvar %in% ivsAME[16:20]] = 4
pDat$group[pDat$prezvar %in% ivsAME[21:25]] = 5
pDat$group[pDat$prezvar %in% c(ivsAME[26:29], "rho")] = 6

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
                              shape = 21, fill = "WHITE")
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


### Function for GLM data

ggCoef2 = function(data, group = NULL)
{
  if(!is.null(group))
  {
    zp1 = ggplot(data[data$group == group, ], aes(color = mod))
  } else{
    zp1 = ggplot(data, aes(color = mod))
  }
  zp1 = zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
  zp1 = zp1 + geom_linerange(aes(x = var, ymin = lo90, ymax = hi90),
                             lwd = 1, position = position_dodge(width = .7))
  zp1 = zp1 + geom_pointrange(aes(x = var, y = mean, ymin = lo95,
                                  ymax = hi95),
                              lwd = 1/2, position = position_dodge(width = .7),
                              shape = 21, fill = "WHITE")
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


## version that drops the dependlow variable,
## to see effects more clearly:

pDatFocused <- pDat[!(pDat$var=="dependlow.dyad"),]

glmBetaFocused <- glmBETA[!(glmBETA$var=="Low Trade Dependence"),]

### Save plots

## All coefficients
ggCoef(data = pDat) ;
ggsave(filename = paste0(resultsPath, 'WeeksAME_coefs.pdf'), device = cairo_pdf, width=7, height=7)

## Dropping dependlow from presentation
ggCoef(data = pDatFocused) ;
ggsave(filename = paste0(resultsPath, 'WeeksAME_MostCoefs.pdf'), device = cairo_pdf, width=7, height=7)

## Just GLM Coefficients
 ggCoef2(data=glmBETA)
ggsave(filename = paste0(resultsPath, 'WeeksGLM_AllCoefs.pdf'), device = cairo_pdf, width=7, height=7)


## version withouth dependlow

ggCoef2(data=glmBetaFocused)
ggsave(filename = paste0(resultsPath, 'WeeksGLM_MostCoefs.pdf'), device = cairo_pdf, width=7, height=7)

## A few at a time
ggCoef(data = pDatFocused, group = 1) ; ggsave(filename = paste0(resultsPath, 'weeks_coefs1.pdf'), device = cairo_pdf, width=7, height=7)
ggCoef(data = pDatFocused, group = 2) ; ggsave(filename = paste0(resultsPath, 'weeks_coefs2.pdf'), device = cairo_pdf, width=7, height=7)
ggCoef(data = pDat, group = 3) ; ggsave(filename = paste0(resultsPath, 'weeks_coefs3.pdf'), device = cairo_pdf, width=7, height=7)
ggCoef(data = pDat, group = 4) ; ggsave(filename = paste0(resultsPath, 'weeks_coefs4.pdf'), device = cairo_pdf, width=7, height=7)

## Conclude
print("End of Coefficient Plot Creation-- check output directory")
