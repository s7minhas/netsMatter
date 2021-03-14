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
