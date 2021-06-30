#############################
# set a path
require(here)
pth = paste0(here::here(), '/')
fPth = paste0(pth, '/helpers/')
wPth = paste0(pth, '2_applications/application_data/weeks/')

# install github from specific repo
# to get same results
# devtools::install_github('s7minhas/amen', ref='pa2018_version')
library(amen)
rZ_bin_fc2 <- dget(paste0(fPth, "rZBinfc.R"))
assignInNamespace("rZ_bin_fc", rZ_bin_fc2, pos="package:amen")

# load in helper functions for ameOutSamp
source(paste0(pth, 'helpers/functions.R'))
loadPkg(c('doParallel','foreach','ROCR', 'RColorBrewer', 'caTools'))
source(paste0(pth, 'helpers/binPerfHelpers.r'))
source(paste0(pth, 'helpers/ameOutSamp.R'))
##############################

##############################
# load data
load(paste0(wPth, 'weeksData.rda'))
load(paste0(wPth, 'ameFitWeeks.rda'))
##############################

##############################
# run out samp code and save ~8 hours
ameOutSamp_k2 = ameOutSamp(
  yList=yList, xDyadL=xDyadList,
  xRowL=xNodeList.s,
  xColL=xNodeList.r,
  R=2, model='bin',
  symmetric=FALSE,
  folds = 30, cores=30, rzBinFix=TRUE, fPth=fPth )
save(ameOutSamp_k2,
  file=paste0(wPth, 'ameOutSampWeeks.rda'))
##############################
