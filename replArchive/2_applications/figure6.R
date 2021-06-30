#############################
# set a path
require(here)
pth = paste0(here::here(), '/')
rsPth = paste0(pth, '2_applications/application_data/reiter_stam/')
wPth = paste0(pth, '2_applications/application_data/weeks/')
gPth = paste0(pth, '2_applications/application_data/gibler/')

# load in helper functions for ameOutSamp
source(paste0(pth, 'helpers/functions.R'))
loadPkg(c('ROCR', 'RColorBrewer', 'caTools'))
source(paste0(pth, 'helpers/binPerfHelpers.r'))
source(paste0(pth, 'helpers/ameOutSamp.R'))
##############################

# get roc and pr curves #############################

# reiter & stam ###########################################
# load data and run if not present
rsFiles = c(
  paste0(rsPth, 'ameOutSampReiterStam.rda'),
  paste0(rsPth, 'glmOutSampReiterStam.rda'))

if(!file.exists(rsFiles[1])){
  source(
    paste0(pth, '2_applications/reiter_stam_ameCrossval.R')) }
load(rsFiles[1])

if(!file.exists(rsFiles[2])){
  source(
    paste0(pth, '2_applications/reiter_stam_glmCrossval.R')) }
load(rsFiles[2])

# org
predDfs = list(
  GLM = data.frame(
    actual=glmPerf$outPerf$actual,
    pred=glmPerf$outPerf$pred, model='GLM'),
  AME = data.frame(
    actual=ameOutSamp_k2$outPerf$actual,
    pred=ameOutSamp_k2$outPerf$pred, model='AME') )

# run
ggPerfCurves(predDfs, 'figure6_reiter_stam', pth)
############################################

# weeks ###########################################
# load data and run if not present
wFiles = c(
  paste0(wPth, 'ameOutSampWeeks.rda'),
  paste0(wPth, 'glmOutSampWeeks.rda'))

if(!file.exists(wFiles[1])){
  source(
    paste0(pth, '2_applications/weeks_ameCrossval.R')) }
load(rsFiles[1])

if(!file.exists(wFiles[2])){
  source(
    paste0(pth, '2_applications/weeks_glmCrossval.R')) }
load(rsFiles[2])

# org
predDfs = list(
  GLM = data.frame(
    actual=glmPerf$outPerf$actual,
    pred=glmPerf$outPerf$pred, model='GLM'),
  AME = data.frame(
    actual=ameOutSamp_k2$outPerf$actual,
    pred=ameOutSamp_k2$outPerf$pred, model='AME') )

# run
ggPerfCurves(predDfs, 'figure6_weeks', pth)
############################################

# gibler ###########################################
# load data and run if not present
gFiles = c(
  paste0(gPth, 'ameOutSampGibler.rda'),
  paste0(gPth, 'glmOutSampGibler.rda'))

if(!file.exists(gFiles[1])){
  source(
    paste0(pth, '2_applications/gibler_ameCrossval.R')) }
load(gFiles[1])

if(!file.exists(gFiles[2])){
  source(
    paste0(pth, '2_applications/gibler_glmCrossval.R')) }
load(gFiles[2])

# org
predDfs = list(
  GLM = data.frame(
    actual=glmPerf$outPerf$actual,
    pred=glmPerf$outPerf$pred, model='GLM'),
  AME = data.frame(
    actual=ameOutSamp_k2$outPerf$actual,
    pred=ameOutSamp_k2$outPerf$pred, model='AME') )

# run
ggPerfCurves(predDfs, 'figure6_gibler', pth)
############################################

##############################
