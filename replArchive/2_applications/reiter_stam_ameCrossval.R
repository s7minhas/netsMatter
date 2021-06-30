#############################
# set a path
require(here)
pth = paste0(here::here(), '/')
rsPth = paste0(pth, '2_applications/application_data/reiter_stam/')

# install github from specific repo
# to get same results
# devtools::install_github('s7minhas/amen', ref='pa2018_version')
library(amen)

# load in helper functions for ameOutSamp
source(paste0(pth, 'helpers/functions.R'))
loadPkg(c('doParallel','foreach','ROCR', 'RColorBrewer', 'caTools'))
source(paste0(pth, 'helpers/binPerfHelpers.r'))
source(paste0(pth, 'helpers/ameOutSamp.R'))
##############################

##############################
# load data
load(paste0(rsPth, 'reiterStamData.rda'))
load(paste0(rsPth, 'ameFitReiterStam.rda'))
##############################

##############################
# run out samp code and save ~14 hours
ameOutSamp_k2 = ameOutSamp(
  yList=yList, xDyadL=xDyadList, xRowL=NULL, xColL=NULL,
  R=2, model='bin',
  folds = 30, cores=10 )
save(ameOutSamp_k2,
  file=paste0(rsPth, 'ameOutSampReiterStam.rda'))
##############################
