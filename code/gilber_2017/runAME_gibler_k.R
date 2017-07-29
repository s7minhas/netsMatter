######### run ame model
#library(devtools)
#devtools::install_github('s7minhas/amen')
if(Sys.info()['user']=='mdw'){
  load('/Users/mdw/Dropbox/research/Ongoing/netsMatter/replications/gibler_2017/amenData_gibler.rda' )
  resultsPath = "/Users/mdw/Dropbox/research/Ongoing/Dropbox/netsMatter/replications/gibler_2017/outputData/"
}

library(amen)

str(yList)
yList[1]

# test run 
brn=200; imps=100; ods=25

# for real
#imps =20000
#brn = 10000
#ods = 25

#### k =0 #####
# params
latDims = 0 ## change k here
seed=6886

# Run amen in parallel

ameFit = ame_repL(
	Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
	model="bin",symmetric=TRUE, # MID DV: undirected data
	intercept=FALSE,R=latDims,
	nscan=imps, seed=seed, burn=brn, odens=ods,
	plot=FALSE, print=FALSE, gof=TRUE,
	periodicSave=TRUE, outFile=paste0(resultsPath,'ameFit_k0.rda')
	)
save(ameFit, file=paste0(resultsPath,'ameFit_k0.rda'))


#### k =1 #####
# params
latDims = 1 ## change k here
seed=6886

# Run amen in parallel

ameFit = ame_repL(
	Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
	model="bin",symmetric=TRUE, # MID DV: undirected data
	intercept=FALSE,R=latDims,
	nscan=imps, seed=seed, burn=brn, odens=ods,
	plot=FALSE, print=FALSE, gof=TRUE,
	periodicSave=TRUE, outFile=paste0(resultsPath,'ameFit_k1.rda')
	)



#### k =2 #####
# params
latDims = 2 ## change k here
seed=6886

# Run amen in parallel

ameFit = ame_repL(
	Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
	model="bin",symmetric=TRUE, # MID DV: undirected data
	intercept=FALSE,R=latDims,
	nscan=imps, seed=seed, burn=brn, odens=ods,
	plot=FALSE, print=FALSE, gof=TRUE,
	periodicSave=TRUE, outFile=paste0(resultsPath,'ameFit_k2.rda')
	)


#### k =3 #####
# params
latDims = 3 ## change k here
seed=6886

# Run amen in parallel

ameFit = ame_repL(
	Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
	model="bin",symmetric=TRUE, # MID DV: undirected data
	intercept=FALSE,R=latDims,
	nscan=imps, seed=seed, burn=brn, odens=ods,
	plot=FALSE, print=FALSE, gof=TRUE,
	periodicSave=TRUE, outFile=paste0(resultsPath,'ameFit_k3.rda')
	)


### parallel
#imps =20000
#brn = 10000
#ods = 25
latDims = 0:3

library(doParallel) ; library(foreach)
cl=makeCluster(4) ; registerDoParallel(cl)

foreach(ii=1:length(latDims), .packages=c("amen")) %dopar% {
  ameFit = ame_repL(
    Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
    model="bin",symmetric=TRUE, # MID DV: undirected data
    R=latDims[ii],
    nscan=imps, seed=seed, burn=brn, odens=ods,
    plot=FALSE, print=FALSE
  )
  save(ameFit, file=paste0(resultsPath, 'ameFit_k', latDims[ii],'.rda'))
  print(ii)
}
