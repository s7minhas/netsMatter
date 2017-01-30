# libraries
# load amen
devtools::install_github('s7minhas/amen') ; library(amen)
resultsPath = '/Users/juantellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/'

# load amen data
load('/Users/juantellez/Desktop/netsMatter-Reiter/Reiter_Stam_2003/output/amenData.rda')

# running in parallel varying k
imps = 25000
brn = 0
ods = 10
latDims = 0:3
seed=6886

prevModelFiles = paste0(resultsPath, 'model_k', latDims,'_v4.rda')
# 



# Run amen in parallel
library(doParallel) ; library(foreach)
cl=makeCluster(4) ; registerDoParallel(cl)
foreach(ii=1:length(latDims), .packages=c("amen")) %dopar% {
  
  # load previous model run
  load(prevModelFiles[ii])
  # extract start vals
  startVals0 = ameFit$'startVals'
  # dump rest
  rm(ameFit)
  
  ameFit = ame_repL(
    Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL, 
    model="bin",symmetric=FALSE,intercept=FALSE,R=latDims[ii], 
    nscan=imps, seed=seed, burn=brn, odens=ods, 
    plot=FALSE, print=FALSE, gof=TRUE, startVals=startVals0,
    periodicSave=TRUE
  ) 	
  save(ameFit, 
       file=paste0(resultsPath, 'model_k', latDims[ii],'_v5.rda')
  )
}
stopCluster(cl)

