## Author: MJF
## Date: 12/26/16
## Purpose: This model replicates the first column of Table 1
## ("cases unweighted by data quality") of Morrow's 2007
## APSR paper.

## The rep data is from his book rep materials (chapter 4 rep files),
## but the table replicates from the paper.


rm(list=ls())
## Setup

if(Sys.info()['user']=='cassydorff'){
    dataPath='~/Dropbox/Research/nothingbutnet/netsMatter/replications/morrow2007/'
    setwd("/Users/cassydorff/")
}
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
    dataPath='~/Dropbox/netsMatter/replications/morrow2007/'
    codePath="~/Research/netsMatter/code/Morrow2007/"
    setwd(codePath)
}


dataPath2 <- '~/Dropbox/netsMatter/replications/morrow2007/Replication/'

loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
	}
    }

loadPkg(c('foreign', 'network', "MASS"))

## MJF Note 12/26-- the "OwAreplicationdata.dta" seems to be STATA 13,
## so needs to be opened with haven

#devtools::install_github("hadley/haven")

require(haven)

dp <- paste0(dataPath2, "Replication/", "OwAreplicationdata.dta")

mdat <- read_dta(paste0(dataPath2, "Replication/", "OwAreplicationdata.dta"))
##dim 1998x 159, which is in the correct ballpark

## best guess is that this is the regression call
## which is the code for table 4.1 in his Ch4 replication.do file

## data condition:data_quality > 0 & WarDeclaration == 0

repdat <- mdat[which(mdat$data_quality > 0 & mdat$WarDeclaration==0),]

## make sure that the dv is a factor for ordered logit call:
repdat$violator_4_ordinal_comply <-  as.factor(repdat$violator_4_ordinal_comply)

dim(repdat) ## 1066 x 159, same as APSR paper

dv <- vars[1]

vars <- c("violator_4_ordinal_comply",
"victim_4_ordinal_comply",	
"victim_clar_4_ordinal_comply", 
"joint_ratify_4_ordinal_recip",
"joint_ratify_clar_recip",
"victim_individual_comply", 
"victim_state_comply", 
"joint_ratify",
"violator_ratified", 
"violator_democracy7",
"joint_ratify_democracy7", 
"violator_ratified_democracy7",
"powerratio",
"joint_ratify_power", 
"Aerial","Armistice",
"CBW",
"Civilians",
"Cultural",
"HighSeas", 
"POWs",
"violator_initiate", 
"violator_deathsper1000population", 
"victim_victor",
"victim_victor_power")

tmp <- paste(vars, collapse=' + ')

tmp

ivs <- vars[2:length(vars)]

# run glm
modForm = formula(paste0(dv, '~',
    paste(vars[2:length(vars)], collapse=' + ')))

library(MASS)

## ordered logit from MASS
mod = polr(modForm, data=repdat, Hess=TRUE, method="probit")

attributes(summary(mod))

round(summary(mod)$coefficients, 3)
