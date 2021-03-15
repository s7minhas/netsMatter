#############################
# set a path
require(here)
pth = paste0(here::here(), '/replArchive/')
rsPth = paste0(pth, '2_applications/application_data/reiter_stam/')

# install github from specific repo
# to get same results
# devtools::install_github('s7minhas/amen', ref='pa2018_version')
library(amen)

# load in helper functions for ameOutSamp
source(paste0(pth, 'helpers/functions.R'))
loadPkg(c('ROCR', 'RColorBrewer', 'caTools'))
source(paste0(pth, 'helpers/binPerfHelpers.r'))
source(paste0(pth, 'helpers/ameOutSamp.R'))
##############################

##############################
# load data
load(paste0(rsPth, 'reiterStamData.rda'))
load(paste0(rsPth, 'ameFitReiterStam.rda'))
startVals0 = ameFit$startVals ; rm(ameFit)
##############################

##############################
# run out samp code and save
startTime = Sys.time()
ameOutSamp_k2 = ameOutSamp(
  yList=yList, xDyadL=xDyadList, xRowL=NULL, xColL=NULL,
  R=2, model='bin',
  startVals=startVals0,
  folds = 30, cores=10 )
endTime = Sys.time()
save(ameOutSamp_k2, startTime, endTime,
  file=paste0(rsPth, 'ameOutSampReiterStam.rda'))
##############################
