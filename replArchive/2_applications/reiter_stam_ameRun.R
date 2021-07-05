#############################
# set a path
require(here)
pth = paste0(here::here(), '/')
rsPth = paste0(pth, '2_applications/application_data/reiter_stam/')

# install github from specific repo
# to get same results
# devtools::install_github('s7minhas/amen', ref='pa2018_version')
library(amen)

# helper functions
source(paste0(pth, 'helpers/ame_repL.R'))
##############################

##############################
# load data
load(paste0(rsPth, 'reiterStamData.rda'))
##############################

##############################
# run and save
# ~1.06 days
ameFit = ame_repL(
  Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
  model="bin",symmetric=FALSE,intercept=TRUE,
  R=2,
  nscan=100000, seed=6886, burn=50000, odens=10,
  plot=FALSE, print=FALSE, gof=TRUE
)

save(ameFit,
  file=paste0(rsPth, 'ameFitReiterStam.rda'))
##############################
