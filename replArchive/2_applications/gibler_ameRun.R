#############################
# set a path
require(here)
pth = paste0(here::here(), '/')
gPth = paste0(pth, '2_applications/application_data/gibler/')

# install github from specific repo
# to get same results
# devtools::install_github('s7minhas/amen', ref='pa2018_version')
library(amen)
library(foreach)
library(doParallel)

# helper functions
source(paste0(pth, 'helpers/ame_repL.R'))
##############################

##############################
# load ame version of data
load(paste0(gPth, 'giblerData.rda'))
##############################

##############################
# run ame and save ~4 days
set.seed(6886)

# commenting out to ensure replication but should return
# the same values set manually in line 24
# seeds = sample(1:6886, 10)
seeds = c(119, 1571, 1922, 211, 3316, 3466, 3508, 4087, 6516, 806)

#
cl=makeCluster(length(seeds)) ; registerDoParallel(cl)
shh = foreach(s = seeds, .packages=c('amen')) %dopar% {
	ameFit = ame_repL(
		Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
		R=2, model="bin",symmetric=TRUE, # MID DV: undirected data
	  nscan=50000, seed=s, burn=25000, odens=10,
		plot=FALSE, print=FALSE, periodicSave=FALSE #, startVals = startVals0
	 )
	save(ameFit,
		file=paste0(gPth, 'ameFitGibler_s',s,'.rda'))
}
stopCluster(cl)
##############################
