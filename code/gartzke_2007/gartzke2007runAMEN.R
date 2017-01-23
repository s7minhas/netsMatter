

if(Sys.info()['user']=='jordan' | Sys.info()['user']=='jordanroberts'){
  source('~/netsMatter/code/mansfield_milner_2012/setup.R') }

# load amen data
if(Sys.info()['user']=='jordan'){
  load('/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/Replication\ specific\ data/gartzke2007amenData.rda')
}

if(Sys.info()['user']=='jordanroberts'){
  load('~/Downloads/gartzke2007amenData.rda')
}






resultsPath = '/Users/jordanroberts/Documents/'


# running just k=0

# params
imps = 100
brn = 200
ods = 10
latDims = 0
seed=1014

# Run amen in parallel
ameFit = ame_repL(
  Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
  model="bin",symmetric=TRUE,intercept=FALSE,R=latDims,
  nscan=imps, seed=seed, burn=brn, odens=ods,
  plot=FALSE, print=FALSE, gof=TRUE,
  periodicSave=TRUE,
  outFile='/Users/jordanroberts/Documents/ameFit_k0.rda'
)
save(ameFit,
     file='/Users/jordanroberts/Documents/ameFit_k0.rda')



# running just k=1

# params
imps = 100
brn = 200
ods = 10
latDims = 1
seed=1014

# Run amen in parallel
ameFit = ame_repL(
  Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
  model="bin",symmetric=TRUE,intercept=FALSE,R=latDims,
  nscan=imps, seed=seed, burn=brn, odens=ods,
  plot=FALSE, print=FALSE, gof=TRUE,
  periodicSave=TRUE,
  outFile='/Users/jordanroberts/Documents/ameFit_k1.rda'
)
save(ameFit,
     file='/Users/jordanroberts/Documents/ameFit_k1.rda')


# running just k=2

# params
imps = 100
brn = 200
ods = 10
latDims = 2
seed=1014

# Run amen in parallel
ameFit = ame_repL(
  Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
  model="bin",symmetric=TRUE,intercept=FALSE,R=latDims,
  nscan=imps, seed=seed, burn=brn, odens=ods,
  plot=FALSE, print=FALSE, gof=TRUE,
  periodicSave=TRUE,
  outFile='/Users/jordanroberts/Documents/ameFit_k2.rda'
)
save(ameFit,
     file='/Users/jordanroberts/Documents/ameFit_k2.rda')


# running just k=3

# params
imps = 100
brn = 200
ods = 10
latDims = 3
seed=1014

# Run amen in parallel
ameFit = ame_repL(
  Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
  model="bin",symmetric=TRUE,intercept=FALSE,R=latDims,
  nscan=imps, seed=seed, burn=brn, odens=ods,
  plot=FALSE, print=FALSE, gof=TRUE,
  periodicSave=TRUE,
  outFile='/Users/jordanroberts/Documents/ameFit_k3.rda'
)
save(ameFit,
     file='/Users/jordanroberts/Documents/ameFit_k3.rda')







# running in parallel varying k

# imps = 1000000
# brn = 500000
# ods = 25
# latDims = 0:3
# seed=6886

imps = 5000
brn = 10000
ods = 25
latDims = 0:3
seed=1014

resultsPath = '/Users/jordanroberts/Documents/'

# Run amen in parallel
library(doParallel) ; library(foreach)
cl=makeCluster(4) ; registerDoParallel(cl)
foreach(ii=1:length(latDims), .packages=c("amen")) %dopar% {
  ameFit = ame_repL(
    Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL, 
    model="bin", symmetric=TRUE,
    R=latDims[ii], 
    nscan=imps, seed=seed, burn=brn, odens=ods, 
    plot=FALSE, print=FALSE
  ) 	
  save(ameFit, 
       file=paste0(resultsPath, 'model_k', latDims[ii],'.rda')
  )
}
stopCluster(cl)


load('/Users/jordanroberts/Documents/model_k3.rda')

paramPlot(ameFit$VC)

paramPlot(ameFit$BETA[,1:6])

gofPlot(ameFit$GOF, symmetric=TRUE)



