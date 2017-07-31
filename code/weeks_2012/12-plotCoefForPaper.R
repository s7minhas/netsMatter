## This script produces coefficient plots for AME output K=2,3 and the GLM model
## According to the specifications posted on Slack

rm(list=ls())

resultsPath = '~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'
inputPath= '~/Dropbox/netsMatter/replications/Weeks2012/replication/input/'
plotPath = '~/Dropbox/netsMatter/replications/0_finalRepFigs/'

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



## load data:
load( paste0(resultsPath,'model_k22017-04-04_v2.rda') ) ; ameFit_k2=ameFit
load( paste0(resultsPath,'model_k32017-03-14_v2.rda') ) ; ameFit_k3=ameFit
load(paste0(inputPath,'weeks_baseModel.rda')); base_mod1=round(modSumm,3) #this is original coefficient estimates

## Meaningful names on IVs:

ivsGLM <- c("Machine", "Junta", "Boss", "Strongman",
         "Other Type","New/Unstable Regime", "Receiver Dem", "Military Capabilities Side1", "Military Capabilities Side2", "Initator Share of Capabilities",
         "Low Trade Dependence", "Both Major Powers",
         "Minor/Major",
         "Major/Minor", "Contiguous", "Log Dist. Between Capitals",
         "Alliance Similarity Side 1",
         "Alliance Similarity Leader",
         "Alliance Similarity Leader Side 2",
         "Time Since Last Conflict", "Spline1", "Spline2", "Spline3")
         

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
glmBETA$prezvar = c("Intercept", ivsGLM)

glmBETA
base_mod1


## AME IVS

## note: 7/29 note: went back into the AMEN build script and verified that
## all independent variables were accounted for. All are there, other than initiator's share of dyadic capabilities. Including this one generated downstream problems with the AMEN build.

ivsAME <- c("Machine_sender", "Junta_sender", "Boss_sender", "Strongman_sender",
         "Other Type_sender","New/Unstable Regime_sender",
            "Military Capabilities Side 1_sender","System Leader Side 1_sender",
            "Machine_receiver",
            "Junta_receiver", "Boss_receiver", "Strongman_receiver", 
            "Other Type_receiver","New/Unstable Regime_receiver",
            "Military Capabilities Side 1_receiver",
            "System Leader Side 1_receiver",
            "Low Trade Dependence_dyad", "Both Major Powers_dyad",
            "Minor/Major_dyad", "Major/Minor_dyad", "Contiguous_dyad",
            "Log Dist_dyad", "AllianceSimilarity_dyad",
            "Time Since Last Conflict_dyad", "Spline1_dyad", "Spline2_dyad", "Spline3_dyad")


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

pDat = rbind(glmBETA, ameBETA2, ameBETA3)

# create groups for plotting
vars = unique(pDat$prezvar)
pDat$group = NA


## Reorder variables so the coef plots look nice
ivsGLM
ivsAME

sort(ivsGLM)
sort(ivsAME)

## set the order of the variables as alphabetical for vars other than the intercept and rho
varOrder = c("Intercept", sort(c(ivsGLM, ivsAME)), "rho")

pDat$prezvar = factor(pDat$prezvar,levels = varOrder, ordered = F)## factor
pDat$prezvar = factor(pDat$prezvar,levels = rev(levels(pDat$prezvar)))## reverse levels needed b/c of how the coordflip rotates


levels(pDat$prezvar)

pDat$group[pDat$prezvar %in% c(varOrder[1:8])] = 1
pDat$group[pDat$prezvar %in% varOrder[9:18]] = 2
pDat$group[pDat$prezvar %in% varOrder[19:27]] = 3
pDat$group[pDat$prezvar %in% varOrder[28:36]] = 4
pDat$group[pDat$prezvar %in% varOrder[37:45]] = 5
pDat$group[pDat$prezvar %in% varOrder[46:52]] = 6

head(pDat)


## plot function for AME models

ggCoefAME = function(data, group = NULL)
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
      shape = 21, fill = "WHITE", fatten=.65)
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

## Take out the splines and Rho:

pDat <- pDat[!( pDat$var=="pcyrsmzinits.dyad"
               |pDat$var=="pcyrsmzinits1.dyad" ##remove the splines from AME
               | pDat$var=="pcyrsmzinits2.dyad"
               | pDat$var=="pcyrsmzinits3.dyad"
               |pDat$var=="pcyrsmzinit" ##remove splines from GLM
               | pDat$var=="pcyrsmzinits1"
               | pDat$var=="pcyrsmzinits2"
               | pDat$var=="pcyrsmzinits3"
               |pDat$var=="rho"),]


### This is removed separately, to dimminish the distorting variable

pDatFocused <- pDat[!(pDat$var=="dependlow.dyad"
                      |pDat$var=="dependlow"),]

### Save plots

## All coefficients
ggCoefAME(data = pDat) ;
ggsave(filename = paste0(plotPath, 'weeks_coefs_all.pdf'),
       device = cairo_pdf, width=7, height=7)

## Dropping dependlow from presentation
ggCoefAME(data = pDatFocused) ;
ggsave(filename = paste0(plotPath, 'weeks_coefs_most.pdf'),
       device = cairo_pdf, width=7, height=7)

#### Some of these credible intervals look extremely small:

pDat[1:30, ]

## check out: alliance similarity dyad, machine receiver/sender, new/unstable,
## strongman receiver/sender

## new idea: take all variables with mean \in [-1, 1] and plot separately:

pDatSmallEffects <- pDat[which(pDat$mean >= -1 &
                               pDat$mean <= 1),]

dim(pDatSmallEffects) ## 59x11

ggCoefAME(dat=pDatSmallEffects)
ggsave(filename = paste0(plotPath, 'weeks_coefs_smallEffects.pdf'),
       device = cairo_pdf, width=7, height=7)

## Conclude

print("End of Coefficient Plot Creation-- check output directory")
