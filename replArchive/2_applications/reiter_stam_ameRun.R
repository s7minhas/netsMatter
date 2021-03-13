#############################
# set a path
require(here)
pth = paste0(here::here(), '/replArchive/')
rsPth = paste0(pth, '2_applications/application_data/reiter_stam/')

# install github from specific repo
# to get same results
# devtools::install_github('s7minhas/amen', ref='pa2018_version')
library(amen)
##############################

##############################
# load data
load(paste0(rsPth, 'reiterStamData.rda'))
##############################

##############################
# run and save
# ~5-6 hours
startTime = Sys.time()
ameFit = ame_repL(
  Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
  model="bin",symmetric=FALSE,intercept=TRUE,
  R=2,
  nscan=10000, seed=6886, burn=25000, odens=10,
  plot=FALSE, print=FALSE, gof=TRUE, startVals=startVals0$startVals,
  periodicSave=TRUE
)
endTime = Sys.time()
print( endTime-startTime )
save(ameFit, startTime, endTime, file=paste0(rsPth, 'ameFitReiterStam.rda'))
##############################
