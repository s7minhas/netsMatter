#############################
# set a path
require(here)
pth = paste0(here::here(), '/')
fPth = paste0(pth, 'helpers/')
wPth = paste0(pth, '2_applications/application_data/weeks/')

# install github from specific repo
# to get same results
# devtools::install_github('s7minhas/amen', ref='pa2018_version')
library(amen)
rZ_bin_fc2 <- dget(paste0(fPth, "rZBinfc.R"))
assignInNamespace("rZ_bin_fc", rZ_bin_fc2, pos="package:amen")

# helper functions
source(paste0(fPth, 'ame_repL.R'))
rZ_bin_fc <- dget(paste0(fPth, "rZBinfc.R"))
##############################

##############################
# load data
load(paste0(wPth, 'weeksData.rda'))
##############################

##############################
# run and save
# ~1.14 days
ameFit = ame_repL(
    Y=yList,
    Xdyad=xDyadList,
    Xrow=xNodeList.s,
    Xcol=xNodeList.r,
    model="bin",
    symmetric=FALSE,
    R=2,
    nscan=100000, seed=6886, burn=50000, odens=25,
    plot=FALSE,
    print=FALSE
)

save(ameFit,
  file=paste0(wPth, 'ameFitWeeks.rda'))
##############################
