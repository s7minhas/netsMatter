#############################
# set a path
require(here)
pth = paste0(here::here(), '/')
fPth = paste0(pth, '/helpers/')
wPth = paste0(pth, '2_applications/application_data/weeks/')

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
load(paste0(wPth, 'weeksData.rda'))

# combine nodal and dyadic covars
xDyadList = lapply(1:length(xDyadList), function(ii){
  xd = xDyadList[[ii]]
  xs = xNodeList.s[[ii]]
  xr = xNodeList.r[[ii]]
  covarArr = amen::design_array_listwisedel(
    xs, xr, xd, FALSE, nrow(xd))
  rownames(covarArr) = colnames(covarArr) = rownames(xd)
  return( covarArr ) })

# cleanup
rm(xNodeList.s, xNodeList.r)
##############################

##############################
glmPerf = glmOutSamp(
  yList=yList, xDyadL=xDyadList,
  seed=6886, folds=30 )
save(glmPerf,
  file=paste0(wPth, 'glmOutSampWeeks.rda'))
##############################
