#############################
# set a path
require(here)
pth = paste0(here::here(), '/')
gPth = paste0(pth, '2_applications/application_data/gibler/')

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
# load ame version of data
load(paste0(gPth, 'giblerData.rda'))
load(paste0(gPth, 'ameFitGibler_s119.rda'))
##############################

##############################
# run out samp code and save
# this model was run on an ec2 instance
ameOutSamp_k2 = ameOutSamp(
  yList=yList, xDyadL=xDyadList, xRowL=NULL, xColL=NULL,
  R=2, model='bin',
  folds = 30, cores=30 )
save(ameOutSamp_k2,
  file=paste0(rsPth, 'ameOutSampGibler.rda'))
##############################
