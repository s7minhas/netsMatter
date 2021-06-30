#############################
# set a path
require(here)
pth = paste0(here::here(), '/')
fPth = paste0(pth, 'helpers/')
rsPth = paste0(pth, '2_applications/application_data/reiter_stam/')

#
source(paste0(fPth, 'loadPkg.R'))
packs = c('reshape2', 'tidyr', 'ROCR', 'RColorBrewer', 'caTools')
loadPkg(packs)

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

#
source(paste0(fPth, 'binPerfHelpers.R'))
source(paste0(fPth, 'glmOutSamp.R'))
##############################

##############################
# load data
load(paste0(rsPth, 'reiterStamData.rda'))
##############################

##############################
glmPerf = glmOutSamp(
  yList=yList, xDyadL=xDyadList,
  seed=6886, folds=30 )
save(glmPerf,
  file=paste0(rsPth, 'glmOutSampReiterStam.rda'))
##############################
