#############################
# set a path
require(here)
pth = paste0(here::here(), '/replArchive/')
gPth = paste0(pth, '2_applications/application_data/gibler/')

# install github from specific repo
# to get same results
# devtools::install_github('s7minhas/amen', ref='pa2018_version')
library(amen)
##############################

##############################
# load ame version of data
load(paste0(gPth, 'giblerData.rda'))
##############################

##############################
# run ame and save ~3 days
startTime = Sys.time()
ameFit = ame_repL(
	Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
	model="bin",symmetric=TRUE, # MID DV: undirected data
	R=2,
	nscan=100000, seed=6886, burn=100, odens=25,
	plot=FALSE, print=FALSE, periodicSave=TRUE,
	startVals = startVals0 )
endTime = Sys.time()
print( endTime-startTime )
save(ameFit, startTime, endTime, file=paste0(gPth, 'ameFitGibler.rda'))
##############################
