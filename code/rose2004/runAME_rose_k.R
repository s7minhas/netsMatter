#### run ame model on amenData_rose.rda #####

# load amen if not installed already
#library(devtools)
#devtools::install_github('s7minhas/amen')

# # 1. mod1= run amen for 200k, burning the first 100k, with odens =25
# 2. check if mod1 has converged
# 3. if mod1 has not converged, input mod1$startVals into mod2 with similar imps, burn, and odens
# 4. check if mod2 has converged
# 5. repeat as necessary

rm(list=ls())

if(Sys.info()['user']=='howardliu'){
  load('/Users/howardliu/Dropbox/netsMatter/replications/rose2004/amenData_rose.rda' )
  resultsPath = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/outputData/sym/"
}


# load pkg
library(amen)




#### k =2 #####
preVersion = 1
prevImps = 50
prevModelFiles = paste0(resultsPath, 'ameFit_k', latDims,'_v',preVersion,'_imps_',prevImps,'_intercept.rda')
load(prevModelFiles)
startVals0 = ameFit$'startVals'

# params
# imps = 50 # v1
# brn  = 25
# version = 1
# ods = 25

imps = 50000 # v2
brn  = 25000 # v2
version = 2 # v2
ods = 25
latDims = 2

seed=6886
version
latDims

ameFit = ame_repL(
  Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
  model="nrm",symmetric=TRUE, # indirected data (sym)
  intercept=TRUE,R=latDims, # fit intercept
  nscan=imps, seed=seed, burn=brn, odens=ods,
  plot=FALSE, print=FALSE, gof=TRUE, 
  startVals = startVals0
)

save(ameFit, file=paste0(resultsPath,'ameFit_k',latDims,'_v',version,'_imps_', imps,'_intercept.rda') )


#### k =3 #####

# set starting Values
# ameFit_k3_v2_imps_20000_intercept.rda
preVersion = 2
prevImps = 20000
prevModelFiles = paste0(resultsPath, 'ameFit_k', latDims,'_v',preVersion,'_imps_',prevImps,'_intercept.rda')
load(prevModelFiles)
startVals0 = ameFit$'startVals'

# params
latDims = 3 ## change k here
seed=6886

ameFit = ame_repL(
  Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
  model="nrm",symmetric=FALSE, # was directed data
  intercept=TRUE,R=latDims, # fit intercept
  nscan=imps, seed=seed, burn=brn, odens=ods,
  plot=FALSE, print=FALSE, gof=TRUE, 
  startVals = startVals0
)

save(ameFit, file=paste0(resultsPath,'ameFit_k',latDims,'_v',version,'_imps_', imps,'_intercept.rda') )

# ### parallel
# #imps = 5000
# #brn = 10000
# #ods = 25
# latDims = 0:3
# 
# library(doParallel) ; library(foreach)
# cl=makeCluster(4) ; registerDoParallel(cl)
# 
# foreach(ii=1:length(latDims), .packages=c("amen")) %dopar% {
#   ameFit = ame_repL(
#     Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
#     model="nrm", symmetric=FALSE,
#     R=latDims[ii],
#     nscan=imps, seed=seed, burn=brn, odens=ods,
#     plot=FALSE, print=FALSE
#   )
#   save(ameFit, file=paste0(resultsPath, 'ameFit_k', latDims[ii],'.rda'))
#   print(ii)
# }
