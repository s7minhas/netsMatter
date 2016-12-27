## Author: MJF
## Date: 12/27/16
## Purpose: Code to produce the AMEN data format
##

## NOTE for all of the Morrow rep files: the rep data is from his book
## rep materials (chapter 4 rep files),
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

## Notes on the data:
## This data has lots of node-level variables
## violator =sender & victim = reciever?
]

## dyadic variables
## "powerratio"                       "joint_ratify"                    
## [97] "joint_ratify_dichotomy"           "Aerial"                          
## [99] "Armistice"                        "CBW"                             
##[101] "Civilians"                        "Cultural"                        
##[103] "HighSeas"                         "POWs"                            
##[105] "WarDeclaration"                   "Wounded"                         
##[107] "data_quality"                     "quality_weights"                 
##[109] "duration"    


## check whether 1 is "full compliance"
summary(repdat$violator_4_ordinal_comply)

table(repdat$violatorccode)

## what dyads do I have?
## would be the  "violatorccode"  and   "victimccode"
## are the end points

## how many unique dyads?
## article says hs has 222 warring directed dyads from 48
## interstate wars

identifiers <- c("violatorccode", "violatorfullname",
"victimfullname", "victimccode", "commonname", "issuearea")

## The following subsets are me working through to see what
## the unique identifier is:

length(unique(repdat$commonname)) #49 conflicts

length(unique(repdat$issuearea)) #8

length(unique(repdat$startyear)) ## 43
length(unique(repdat$endyear)) ## 42

table(repdat$startyear)
table(repdat$endyear)

justcountries <- c("violatorccode", "violatorfullname",
"victimfullname", "victimccode")

test <- unique(repdat[,justcountries])

dim(test) ## 170 x 4

##  Unit in the paper is "directed warring dyad-issue area"
    
uniqueDyads <- unique(repdat[, identifiers]) ## 170 x 2

dim(uniqueDyads)


