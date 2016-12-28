# Author: CD
## Purpose (old notes): This code creates a dataframe for OLS & oprobit replication for the last time period, issue area POWs.
##This data frame is more compatible with the GBME results and doesn't have the same selection issues as original dataframe / approach.

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

loadPkg(c('foreign', 'network'))

######################
## Data
########################
## The following seem to be for table 5?
## mjf notes 12/26-- not sure what this data is. Why only 20 rows?
## maybe table 5 of the document?

## Read in data which contains dyadic compliance measure
morrow.pows.t5.dat<-read.dta(paste0(dataPath,"morrow_pows_t5.dta"))
## data frame of dim 20 x167

## sender democracy
morrow.pows.t5.dem<-read.dta(paste0(dataPath,"morrow_pows_t5_dem.dta"))
## 16x3 dataframe, 

## dyadic ally data
morrow.pows.t5.ally<-read.dta(
    paste0(dataPath,"morrow_pows_t5_ally.dta")) ##11x6 dataframe

colnames(morrow.pows.t5.ally)

##not sure why I made this in 2012 but i did:
ally<-c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0)

## OLS

## MJF note: 12/26-- "ally" isnt' a variable name in this dataframe
morrow.lm<-lm(violator_4_ordinal_comply~
              violator_democracy7 + victim_4_ordinal_comply + ally,
              data=morrow.pows.t5.dat)

morrow.lm<-lm(violator_4_ordinal_comply~
              violator_democracy7 +victim_4_ordinal_comply + ally,
              data=morrow.pows.t5.dat)
######################


##plots
# Comply varies, dem =1, ally=1 (only thing that actually makes some sense)
data.dem.comp<-data.frame(violator_democracy7=1,
                          victim_4_ordinal_comply=morrow.pows.t5.dat$victim_4_ordinal_comply, ally=1)

p.comp<-data.frame(predict(morrow.lm, data.dem.comp, interval="confidence"))

plot(data.dem.comp$victim_4_ordinal_comply, p.comp$fit, type="l", ylim=c(0,6))
lines(data.dem.comp$victim_4_ordinal_comply, p.comp$lwr, type="l", col="blue")
lines(data.dem.comp$victim_4_ordinal_comply, p.comp$upr, type="l", col="blue") 
######################

#####################
#####################
## MJF additions 12/26
## other morrow data from his replication files


## MJF Note 12/26-- the "OwAreplicationdata.dta" seems to be STATA 13,
## so needs to be opened with haven

#devtools::install_github("hadley/haven")

require(haven)

dp <- paste0(dataPath2, "Replication/", "OwAreplicationdata.dta")

mdat <- read_dta(paste0(dataPath2, "Replication/", "OwAreplicationdata.dta"))
##dim 1998x 159, which is in the correct ballpark

## best guess is that this is the regression call
## which is the code for table 4.1 in his Ch4 replication.do file

## oprob violator_4_ordinal_comply victim_4_ordinal_comply victim_clar_4_ordinal_comply joint_ratify_4_ordinal_recip joint_ratify_clar_recip victim_individual_comply victim_state_comply joint_ratify  violator_ratified violator_democracy7 joint_ratify_democracy7 violator_ratified_democracy7 powerratio joint_ratify_power Aerial Armistice CBW Civilians Cultural HighSeas POWs violator_initiate violator_deathsper1000population victim_victor victim_victor_power if data_quality > 0 & WarDeclaration == 0

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
