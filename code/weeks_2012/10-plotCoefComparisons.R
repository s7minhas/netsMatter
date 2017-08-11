## This script produces coefficient plots for AME output K=0:3 and the GLM model
## Script 1 of the model presentation 
##Based on Juan's code for Reiter-Stam

rm(list=ls())

## This assumes that I'm working on my desktop or laptop

resultsPath = '~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'
inputPath= '~/Dropbox/netsMatter/replications/Weeks2012/replication/input/'
plotPath = '~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'

### libraries needed

source('helperEx.R')

toLoad <- c("dplyr", "magrittr", "ggplot2", "stringr", "gridExtra",
            "Cairo", "reshape2")

loadPkg(toLoad)

# load data
load( paste0(resultsPath,'model_k0.rda') ) ; ameFit_k0=ameFit
load( paste0(resultsPath,'model_k1.rda') ) ; ameFit_k1=ameFit
load( paste0(resultsPath,'model_k2.rda') ) ; ameFit_k2=ameFit
load( paste0(resultsPath,'model_k3.rda') ) ; ameFit_k3=ameFit

load(paste0(inputPath,'weeks_baseModel.rda')); base_mod1=round(modSumm,3) #this is original coefficient estimates

## Meaningful names on IVs:

ivsGLM <-  c("Machine", "Junta", "Boss", "Strongman",
             "Other Type","New/Unstable Regime", "Democracy_Reciever",
             "Military Capabilities_Initiator",
             "Military Capabilities_Reciever",
             "Initator Share of Capabilities",
             "Low Trade Dependence", "Both Major Powers",
             "Minor/Major",
             "Major/Minor", "Contiguous", "Log Dist. Between Capitals",
             "Alliance Similarity_Dyad",
             "Alliance Similarity Syst Leader_Initiator ",
             "Alliance Similarity Leader_Reciever",
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

ivsAME <- c("Machine_sender", "Junta_sender", "Boss_sender",
           "Strongman_sender",
           "Other Type_sender","New/Unstable Regime_sender",
           "Military Capabilities_sender","System Leader_sender",
           "Democracy_reciever", "Military Capabilities_reciever",
           "System Leader_reciever",
           "Low Trade Dependence_dyad", "Both Major Powers_dyad",
           "Minor/Major_dyad", "Major/Minor_dyad", "Contiguous_dyad",
           "Log Dist_dyad", "AllianceSimilarity_dyad",
           "Time Since Last Conflict_dyad", "Spline1_dyad", "Spline2_dyad", "Spline3_dyad")

ivsAME

## AME K0
ameBETA = cbind(ameFit_k0$BETA, rho = ameFit_k0$VC[,'rho'])
ameBETA = t(apply(ameBETA, 2, summStats))
colnames(ameBETA) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA = data.frame(ameBETA, stringsAsFactors = F)
ameBETA$var = rownames(ameBETA) ; rownames(ameBETA) = NULL
ameBETA$mod = 'AME_K0'
ameBETA$prezvar = c("Intercept", ivsAME, "rho")

# AME K1
ameBETA1 = cbind(ameFit_k1$BETA, rho = ameFit_k1$VC[,'rho'])
ameBETA1 = t(apply(ameBETA1, 2, summStats))
colnames(ameBETA1) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA1 = data.frame(ameBETA1, stringsAsFactors = F)
ameBETA1$var = rownames(ameBETA1) ; rownames(ameBETA1) = NULL
ameBETA1$mod = 'AME_K1'
ameBETA1$prezvar = c("Intercept",ivsAME, "rho")

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

pDat = rbind(glmBETA, ameBETA, ameBETA1, ameBETA2, ameBETA3)

# create groups for plotting
vars = unique(pDat$prezvar)
pDat$group = NA


sort(ivsGLM)
sort(ivsAME)

## set the order of the variables as alphabetical for vars other than the intercept and rho
varOrder = c("Intercept", sort(c(ivsGLM, ivsAME)), "rho")

pDat$prezvar = factor(pDat$prezvar,levels = varOrder, ordered = F)## factor
pDat$prezvar = factor(pDat$prezvar,levels = rev(levels(pDat$prezvar)))## reverse levels needed b/c of how the coordflip rotates

unique(pDat$prezvar)

length(unique(varOrder)) #41

levels(pDat$prezvar)

pDat$group[pDat$prezvar %in% c(varOrder[1:10])] = 1
pDat$group[pDat$prezvar %in% varOrder[11:20]] = 2
pDat$group[pDat$prezvar %in% varOrder[21:30]] = 3
pDat$group[pDat$prezvar %in% varOrder[31:40]] = 4
pDat$group[pDat$prezvar %in% varOrder[41:47]] = 5

head(pDat)



# plot function for AME models
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
      shape = 21, fill = "WHITE"
      #, fatten=.75
      )
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

ggCoefGLM = function(data, group = NULL)
{
  if(!is.null(group))
  {
    zp1 = ggplot(data[data$group == group, ], aes(color = mod))
  } else{
    zp1 = ggplot(data, aes(color = mod))
  }
  zp1 = zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) #intercept line
  zp1 = zp1 + geom_linerange(aes(x = var, ymin = lo90, ymax = hi90),##90/% CI
                             lwd = 1, position = position_dodge(width = .7))
  zp1 = zp1 + geom_pointrange(aes(x = var, y = mean, ymin = lo95,
                                  ymax = hi95), ##95/% CI and point
                              lwd = 1/2, position = position_dodge(width = .7),
      shape = 21, fill = "WHITE", fatten=.5) #sets dot size and shape 
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

dim(pDat)

pDat <- pDat[!( pDat$var=="pcyrsmzinits.dyad"
               |pDat$var=="pcyrsmzinits1.dyad" ##remove the splines from AME
               | pDat$var=="pcyrsmzinits2.dyad"
               | pDat$var=="pcyrsmzinits3.dyad"
               |pDat$var=="pcyrsmzinits" ##remove splines from GLM
               | pDat$var=="pcyrsmzinits1"
               | pDat$var=="pcyrsmzinits2"
               | pDat$var=="pcyrsmzinits3"
               |pDat$var=="rho"),]

glmBETA

glmBETA <- glmBETA[!(glmBETA$prezvar=="Time Since Last Conflict"
                     |glmBETA$prezvar=="Spline1" ##also remove the splines
                     | glmBETA$prezvar=="Spline2"
                     | glmBETA$prezvar=="Spline3"),]


### This is removed separately, to dimminish the distorting variable

pDatFocused <- pDat[!(pDat$var=="dependlow.dyad"
                      |pDat$var=="dependlow"),]

glmBetaFocused <- glmBETA[!(glmBETA$prezvar=="Low Trade Dependence"),]

### Save plots

## All coefficients
ggCoefAME(data = pDat) ;
ggsave(filename = paste0(resultsPath, 'WeeksAME_coefs.pdf'), device = cairo_pdf, width=7, height=7)

## Dropping dependlow from presentation
ggCoefAME(data = pDatFocused) ;
ggsave(filename = paste0(resultsPath, 'WeeksAME_MostCoefs.pdf'), device = cairo_pdf, width=7, height=7)

## Just GLM Coefficients
ggCoefGLM(data=glmBETA)
ggsave(filename = paste0(resultsPath, 'WeeksGLM_AllCoefs.pdf'), device = cairo_pdf, width=7, height=7)

## version withouth dependlow

ggCoefGLM(data=glmBetaFocused)
ggsave(filename = paste0(resultsPath, 'WeeksGLM_MostCoefs.pdf'), device = cairo_pdf, width=7, height=7)

## Conclude
print("End of Coefficient Plot Creation-- check output directory")
