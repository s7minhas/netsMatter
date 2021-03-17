#############################
# set a path
require(here)
pth = paste0(here::here(), '/replArchive/')
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
##############################

##############################
# load data
load(paste0(wPth, 'weeksData.rda'))
##############################
