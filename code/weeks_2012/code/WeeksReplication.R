## This file to replicate Jessica Weeks'

rm(list=ls())

############################
## Initialization
############################

loadPkg=function(toLoad){
  for(lib in toLoad){
  if(! lib %in% installed.packages()[,1])
    { install.packages(lib, repos='http://cran.rstudio.com/') }
  suppressMessages( library(lib, character.only=TRUE) ) }}

#### probably longer, but more easy to read:

install.packages("clusterSEs")

visualization <- c("ggplot2")
datawrangling=c('foreign', "scales",'data.table',
    "lubridate", "tidyr", "lmtest",
    "multiwayvcov")

packs=c(visualization, datawrangling)

loadPkg(packs)

#########################
## Setup
#########################

dataPath <- "~/Dropbox/netsMatter/replications/Weeks2012/replication/data/"

### Helper functions

logit.cluster <- dget("logitClusteredSE.R")

lmtest','multiwayvcov
library(foreign)

data <- read.dta(paste0(dataPath, "WeeksAPSR2012.dta"))

head(data) #looks like data

colnames(data)

summary(data$mzinit)
table(data$mzinit) #1,03,7473 0, 1811 yes.

## Logit call on the replication file used
## the data for which democracy_1 variable is
## not missing.

## So, subset data to be only
## the variables in the regression and for which
## there is not missingness in democracy_1.

regressionVars <- c("mzinit", "machinejlw_1", "juntajlw_1",
                    "bossjlw_1", "strongmanjlw_1", "allotherauts_1",
                    "newregime_1", "democracy_2", "cap_1",
                    "cap_2", "initshare", "dependlow", "majmaj",
                    "minmaj", "majmin", "contigdum", "logdist",
                    "s_wt_glo", "s_lead_1", "s_lead_2", "pcyrsmzinit",
                    "pcyrsmzinits1", "pcyrsmzinits2", "pcyrsmzinits3")


dataSub <- data[regressionVars]

dataAsPaper <- data[which(!is.na(data$democracy_1)),]

length(which(is.na(dataSub))) # Lots of NA:271049

## just calling the glm() logit model, without doing the
## "if democracy_1!=.," call

dv <- regressionVars[1]
ivs <- regressionVars[2:length(regressionVars)]

modElements=formula(paste0( dv, '~ ', paste(ivs, collapse=' + ') ))

## translating the synatax of her *1.2 in the replication file:
naieveRep <-glm(modElements, family=binomial(link=logit),
                data=dataAsPaper)

round(summary(naieveRep)$'coefficients',3)


##logit.cluster takes estimated model and variable
## attribute to cluster on.

loadPkg(c('lmtest','multiwayvcov'))

baseModelVcov = cluster.vcov(model=naieveRep,
    cluster=dataAsPaper$dirdyadid, 
    df_correction = FALSE, leverage = 3)

baseModelSumm = coeftest(baseModel, baseModelVcov)

##########################
#### VERSION WITH NA OMIT
#########################

dataTake2 <- na.omit(dataAsPaper)

dim(dataAsPaper)
dim(dataTake2)

modRep <-glm(modElements,
             family=binomial(link=logit),
             data=dataTake2)

round(summary(modRep)$'coefficients',3)
## but, a bit more detail on the data:

############################
## Some descriptives about the data
###########################
class(dataAsPaper)

dim(dataAsPaper)

colnames(dataAsPaper)

## how many countries:


length(unique(dataAsPaper$ccode1)) #165
length(unique(dataAsPaper$ccode2)) #197

## DV:

table(dataAsPaper$mzinit) #1,737 initiated conflicts
## 899,933 non-initiated dyads


## what do we see among the conflicts:

mids <- dataAsPaper[which(dataAsPaper$mzinit==1),]

dim(mids)

length(unique(mids$ccode1)) #140

length(unique(mids$ccode2)) #154

table(mids$ccode1)

hist(table(mids$ccode1))

## code 365 had 144 mids. Is Russia;
##710 had 101; is China
##2 had 97; is US.
