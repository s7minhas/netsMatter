if(Sys.info()['user']=='jordan'){
  source('~/netsMatter/code/mansfield_milner_2012/setup.R') }

# load amen data
load('/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/Replication\ specific\ data/gartzke2007amenData.rda')

# running in parallel varying k
# imps = 1000000
# brn = 500000
# ods = 25
# latDims = 0:3
# seed=6886

imps = 1000
brn = 2000
ods = 10
latDims = 0:3
seed=1014

# Run amen in parallel
library(doParallel) ; library(foreach)
cl=makeCluster(4) ; registerDoParallel(cl)
foreach(ii=1:length(latDims), .packages=c("amen")) %dopar% {
  ameFit = ame_repL(
    Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL, 
    model="bin", symmetric=TRUE,
    R=latDims[ii], 
    nscan=imps, seed=seed, burn=brn, odens=ods, 
    plot=FALSE, print=FALSE
  ) 	
  save(ameFit, 
       file=paste0(resultsPath, 'model_k', latDims[ii],'.rda')
  )
}
stopCluster(cl)





