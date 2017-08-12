## This script produces coefficient plots for AME output K=2,3 and the GLM model
## According to the specifications posted on Slack

rm(list=ls())

resultsPath = '~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'
inputPath= '~/Dropbox/netsMatter/replications/Weeks2012/replication/input/'
plotPath = '~/Dropbox/netsMatter/replications/0_finalRepFigs/'

### libraries needed

source('helperEx.R')

toLoad <- c("dplyr", "magrittr", "ggplot2", "stringr", "gridExtra",
            "Cairo", "reshape2", 'amen')

loadPkg(toLoad)
## load data:
load( paste0(resultsPath,'model_k2.rda') ) ; ameFit_k2=ameFit
load(paste0(inputPath,'weeks_baseModel.rda')); base_mod1=round(modSumm,3) #this is original coefficient estimates

## Meaningful names on IVs:

ivsGLM <- c("Machine", "Junta", "Boss", "Strongman",
            "Other Type","New/Unstable Regime", "Democracy_Target",
            "Military Capabilities_Initiator",
            "Military Capabilities_Target ",
            "Initator Share of Capabilities ",## remember this has no AMEN equivalent
            "Low Trade Dependence ",
            "Both Major Powers", "Minor/Major",
            "Major/Minor", "Contiguous", "Log Dist. Between Capitals",
            "Alliance Similarity_Dyad ",
            "Alliance Similarity With System Leader_Initiator",
            "Alliance Similarity Leader_Target",
            "Time Since Last Conflict", "Spline1", "Spline2", "Spline3")


# sum stats
summStats = function(x){
  res=c(mu=mean(x),med=median(x),sd=sd(x),quantile(x,probs=c(0.025,0.05,0.95,0.975)))
  round(res, 3)
}

## Point estimates for the GLM model

glmBETA = data.frame(var = as.character(rownames(base_mod1)), 
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

glmBETA$var = factor(glmBETA$var,levels = rownames(base_mod1), ordered = F)## factor

glmBETA$var
rownames(base_mod1)

## AME IVS

## note: 7/29 note: went back into the AMEN build script and verified that
## all independent variables were accounted for. All are there, other than initiator's share of dyadic capabilities. Including this one generated downstream problems with the AMEN build.

# AME K2

ameVarsLevels <- c("intercept", "machinejlw_1.row", "juntajlw_1.row",  "bossjlw_1.row", "strongmanjlw_1.row", "allotherauts_1.row", "newregime_1.row", "democracy_2.col", "cap_1.row", "cap_2.col","dependlow.dyad","majmaj.dyad","minmaj.dyad", "majmin.dyad", "contigdum.dyad","logdist.dyad","s_wt_glo.dyad", "s_lead_1.row", "s_lead_2.col","pcyrsmzinit.dyad", "pcyrsmzinits1.dyad", "pcyrsmzinits2.dyad", "pcyrsmzinits3.dyad")


ivsAME <- c("Machine", "Junta", "Boss", "Strongman",
            "Other Type","New/Unstable Regime", "Democracy_Target",
            "Military Capabilities_Initiator",
            "Military Capabilities_Target ",
            ##"Initator Share of Capabilities ",##isn't in AME build
            "Low Trade Dependence ",
            "Both Major Powers", "Minor/Major",
            "Major/Minor", "Contiguous", "Log Dist. Between Capitals",
            "Alliance Similarity_Dyad ",
            "Alliance Similarity With System Leader_Initiator",
            "Alliance Similarity Leader_Target",
            "Time Since Last Conflict", "Spline1", "Spline2", "Spline3")


ameBETA2 = cbind(ameFit_k2$BETA, rho = ameFit_k2$VC[,'rho'])
ameBETA2 = t(apply(ameBETA2, 2, summStats))
colnames(ameBETA2) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA2 = data.frame(ameBETA2, stringsAsFactors = F)
ameBETA2$var = rownames(ameBETA2) ; rownames(ameBETA2) = NULL
ameBETA2$mod = 'AME_K2'
ameBETA2$prezvar =  c("Intercept", ivsAME, "rho")

ameBETA2$var = factor(ameBETA2$var,levels = c(ameVarsLevels, "rho"),
    ordered = F)## factor


levels(glmBETA$var)
ameBETA2$var

dim(ameBETA2)
dim(glmBETA)

colnames(ameBETA2)
colnames(glmBETA)

## combine and clean up


pDat = rbind(glmBETA, ameBETA2)

# create groups for plotting
vars = unique(pDat$prezvar)
pDat$group = NA


## Reorder variables so the coef plots look nice

pDat$prezvar = factor(pDat$prezvar,levels = c("Intercept", ivsGLM, "rho"),
    ordered = F)## factor

pDat$prezvar = factor(pDat$prezvar,levels = rev(levels(pDat$prezvar)))## reverse levels needed b/c of how the coordflip rotates



levels(pDat$prezvar)

length(unique(levels(pDat$prezvar)))
           
pDat

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
  zp1 = zp1 + scale_color_brewer(palette = 'Set1')+ theme_bw()
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

pDat <- pDat[!( pDat$var=="pcyrsmzinit.dyad"
               |pDat$var=="pcyrsmzinits"
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

dim(pDatSmallEffects) ## 31x11

ggCoefAME(dat=pDatSmallEffects)
ggsave(filename = paste0(plotPath, 'weeks_coefs_smallEffects.pdf'),
       device = cairo_pdf, width=7, height=7)

## Conclude

print("End of Coefficient Plot Creation-- check output directory")

