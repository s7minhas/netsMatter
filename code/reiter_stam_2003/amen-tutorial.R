## Task: AMEN model tutorial using replication data from Reiter_Stam_2003
## Date: January 2020

################################################################################
################################################################################
################################################################################

## clear workspace
rm(list=ls())

## setup working directory
setwd('code/reiter_stam_2003')

## load libraries
packs = c('dplyr', 'ggplot2', 'foreign', 'readr', 'lmtest','magrittr')
source("LoadPkg.R")
loadPkg(packs)

################################################################################
## format data
################################################################################

## read data and add variables
data = read.dta('Reiter_Stam_2003/input/doublereduce.dta')

## get variables needed
base_vars = c('sideaa pdemdtar pdemdin personal military single democ contig majpow ally loglsrat advanced dispyrs dspline1 dspline2 dspline3 statea stateb year') %>% 
  strsplit(x = ., split = " ") %>%  unlist()

dyad_vars = c('personal', 'military', 'single', 'democ', 'contig', 
              'ally', 'majpow', 'loglsrat', 'advanced', 
              'dispyrs', 'dspline1', 'dspline2', 'dspline3', 
              'pdemdtar', 'pdemdin')

## subset the data by variables needed and years
dataComp = na.omit(data[, base_vars])
yrs = sort(unique(dataComp$year)) 
cntriesT = lapply(yrs, function(t){ as.character( unique( dataComp$statea[dataComp$year==t] ) ) })

## format dependent variable
yVar = 'sideaa'
yList = lapply(1:length(yrs), function(ii){
  slice = data[ which( 
    data$year==yrs[ii] & 
      data$statea %in% cntriesT[[ii]] & 
      data$stateb %in% cntriesT[[ii]]
  ), c('statea', 'stateb', yVar) ]
  adj = reshape2::acast(slice, statea ~ stateb, value.var=yVar)
  return( adj[ cntriesT[[ii]], cntriesT[[ii]] ] )
})

## format dyadic covariates
xDyadList = lapply(1:length(yrs), function(ii){
  slice = data[ which( 
    data$year==yrs[ii] & 
      data$statea %in% cntriesT[[ii]] & 
      data$stateb %in% cntriesT[[ii]]
  ), c('statea', 'stateb', dyad_vars) ]
  sliceL = reshape2::melt(slice, id=c('statea','stateb'))
  adj = reshape2::acast(sliceL, statea ~ stateb ~ variable, value.var='value')
  return( adj[ cntriesT[[ii]], cntriesT[[ii]],  ] )
})
## --> now, we are done with data formatting and it's ready for amen


################################################################################
## Run amen model
################################################################################

## load most updated amen package
devtools::install_github('s7minhas/amen') ; library(amen)

## setup result output directory
resultsPath = 'resultsPath'

## running in parallel varying k
imps = 10000  # number of interations in the MCMC chain
brn =  2500   # number of burn-in
ods = 1       # number of thinning in the MCMC chain 
latDims = 0:3 # dimension of latent space (search up to 3 dimension is suggested)
seed=6886 # set your model seed

## load resuls from previous model run if needed
prevModelFiles = paste0(resultsPath, 'model_k', latDims,'_v1.rda')


## Run amen in parallel
library(doParallel) ; library(foreach)

## select number of cores to locate
cl=makeCluster(4) ; registerDoParallel(cl)

foreach(ii=1:length(latDims), .packages=c("amen")) %dopar% {
  
  load(prevModelFiles[ii]) # load previous model run
  
  startVals0 = ameFit$'startVals' # extract start vals
 
  rm(ameFit)  # dump the rest of the model run 
  
  ameFit = ame_repL(
    Y=yList,Xdyad=xDyadList, 
    Xrow=NULL,Xcol=NULL, 
    model="bin",symmetric=FALSE,intercept=TRUE,R=latDims[ii], 
    nscan=imps, seed=seed, burn=brn, odens=ods, 
    plot=FALSE, print=FALSE, gof=TRUE, startVals=startVals0,
    periodicSave=TRUE) 	
  
  save(ameFit, file=paste0(resultsPath, 'model_k', latDims[ii],'_v2.rda') ) # save the output of your data
}

## terminate parallel sockets
stopCluster(cl)

## --> now, we've finished model fitting and are ready to examine model fitness and convergence

################################################################################
## posterior analysis
################################################################################

resultsPath = 'resultsPath'

## load model fits from 0 to 3 dimensions
load( paste0(resultsPath,'ameFit_k0.rda') ) ; ameFit_k0=ameFit
load( paste0(resultsPath,'ameFit_k1.rda') ) ; ameFit_k1=ameFit
load( paste0(resultsPath,'ameFit_k2.rda') ) ; ameFit_k2=ameFit
load( paste0(resultsPath,'ameFit_k3.rda') ) ; ameFit_k3=ameFit

## check out goodness of fit stats in posterior estimate for each dimension
amen::gofPlot(ameFit_k0$GOF, symmetric = TRUE)
amen::gofPlot(ameFit_k1$GOF, symmetric = TRUE)
amen::gofPlot(ameFit_k2$GOF, symmetric = TRUE)
amen::gofPlot(ameFit_k3$GOF, symmetric = TRUE)

## check out beta parameters (posterior estimates for coefficients) and convergence for each dimension
summStats = function(x){
  res=c(mu=mean(x),med=median(x),sd=sd(x),quantile(x,probs=c(0.025,0.05,0.95,0.975))) # credible intervals
  round(res, 3)
}
apply(ameFit_k0$BETA, 2, summStats) %>% t() %>% nrow()
apply(ameFit_k1$BETA, 2, summStats) %>% t() %>% nrow()
apply(ameFit_k2$BETA, 2, summStats) %>% t() %>% nrow()
apply(ameFit_k3$BETA, 2, summStats) %>% t() %>% nrow()

## check the parameters convergence for  the sender and receiver effects 
betaIndices<-split(1:ncol(ameFit_k0$BETA), ceiling(seq_along(1:ncol(ameFit_k0$BETA))/5))

## plot the parameters convergence using dimension k=0
for(bIndex in betaIndices){ amen::paramPlot( ameFit_k0$BETA[,bIndex,drop=FALSE] ) }
amen::paramPlot(ameFit_k0$VC)


