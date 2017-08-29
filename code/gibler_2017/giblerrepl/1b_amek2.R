rm(list = ls())

if(Sys.info()['user']=='s7m'){
	dropPath = '~/Dropbox/Research/netsMatter/replications/gibler_2017/'
	resultsPath = paste0(dropPath, 'outputData/')	
}

# load data and past results
library(amen)
load(paste0(dropPath, "amenData_gibler2.rda"))
load(paste0(resultsPath, 'mdwameFit_k2.rda'))

# access startVals
startVals0 = ameFit$startVals
rm(ameFit)

# for real
imps = 50000 
brn  = 25000 
ods = 25

#### k =2 #####
# params
seed=6886

ameFit = ame_repL(
	Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
	model="bin",symmetric=TRUE, # MID DV: undirected data
	R=2,
	nscan=imps, seed=seed, burn=brn, odens=ods,
	plot=FALSE, print=FALSE, 
	startVals = startVals0
)

save(ameFit, file='mdwameFit_k2_v2.rda')