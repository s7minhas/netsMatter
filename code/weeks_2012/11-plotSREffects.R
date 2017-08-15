## This script produces sender and reciever
## random effects for k=2
## ordered by size

rm(list=ls())

### libraries needed

source('../reiter_stam_2003/helperEx.R')
source('../McDonald_2004/setup.R')

toLoad <- c("RColorBrewer",
"dplyr",
"magrittr",
"ggplot2",
"stringr",
"gridExtra",
"Cairo",
            "reshape2")

loadPkg(toLoad)
### Load data

################################################
## Weeks random effects:
## Should be: ordered by size, on a white background
##############################################

## Load K2 data:
load('~/Dropbox/netsMatter/replications/Weeks2012/replication/output/model_k2.rda')

weeksPath <- '~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'


####

effdat = getAddEffData(fit = ameFit) ##This function is in helperEx.R
effdat$actor = countrycode::countrycode(effdat$actor, 'cown', 'country.name')

effdat$actor = factor(effdat$actor, 
      levels=effdat[order(effdat$addEff),'actor'])

effdatSorted <- effdat[order(effdat$addEff),]

head(effdat)
head(effdatSorted)
class(effdat$actor)

## All countries

addEffPlot(fit = effdat, addEffData = T, row = T)
ggsave(filename = paste0(weeksPath, 'Weeks_sender_k2.pdf'), device = cairo_pdf, width=7, height=7)

##lowest 20

addEffPlot(fit = effdatSorted[1:20,], addEffData = T, row = T)
ggsave(filename = paste0(weeksPath, 'Weeks_low20_sender_k2.pdf'),
       device = cairo_pdf, width=7, height=7)


## Highest 20
addEffPlot(fit = effdatSorted[106:125,], addEffData = T, row = T)
ggsave(filename = paste0(weeksPath, 'Weeks_top20_sender_k2.pdf'),
       device = cairo_pdf, width=7, height=7)

#########
## Weeks Reciever:
###########

####
recdat = getAddEffData(fit = ameFit, row=FALSE) 
recdat$actor = countrycode::countrycode(recdat$actor, 'cown',
    'country.name')

recdat$actor = factor(recdat$actor, 
      levels=recdat[order(recdat$addEff),'actor'])

recdatSorted <- recdat[order(recdat$addEff),]

head(recdat)
head(recdatSorted)
## All countries

addEffPlot(fit = recdat, addEffData=TRUE, row = FALSE)
ggsave(filename = paste0(weeksPath, 'Weeks_rec_k2.pdf'), device = cairo_pdf, width=7, height=7)

##lowest 20

addEffPlot(fit = recdatSorted[1:20,], addEffData = T, row = F)
ggsave(filename = paste0(weeksPath, 'Weeks_low20_rec_k2.pdf'),
       device = cairo_pdf, width=7, height=7)


## Highest 20
addEffPlot(fit = recdatSorted[106:125,], addEffData = T, row = F)

ggsave(filename = paste0(weeksPath, 'Weeks_top20_sender_k2.pdf'),
       device = cairo_pdf, width=7, height=7)
