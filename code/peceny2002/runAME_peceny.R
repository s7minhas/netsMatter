## run ame model
#library(devtools)
#devtools::install_github('s7minhas/amen')
library(amen)

# load data
dataPath = "/Users/howardliu/Dropbox/netsMatter/replications/peceny2002/"
dataPath = "/Users/maxgallop/Dropbox (netgroup)/netsMatter/replications/peceny2002/"

load( paste0(dataPath, 'amenData_peceny.rda') )

#
resultsPath = "/Users/maxgallop/desktop/"

str(yList)
yList[1]
# params

#imps = 50000
#brn = 100000

imps = 5
brn = 3
ods = 1
latDims = 0
seed=6886


for(i in 1:length(yList)){
  x = yList[[i]]
  x[lower.tri(x)] = 0
  x = x + t(x)
  yList[[i]] = x
  rm(x)
}


for(i in 1:length(xDyadList)){
  for(j in 1:dim(xDyadList[[i]])[3]){
  x = xDyadList[[i]][,,j]
  x[lower.tri(x)] = 0
  x = x + t(x)
  xDyadList[[i]][,,j] = x
  rm(x)
}}

for(i in 1:length(xDyadList)){
  for(j in 1:dim(xDyadList[[i]])[3]){
    x = xDyadList[[i]][,,j]
if(isSymmetric(x) == 0){print("shit")}
      }}

# Run amen in parallel

ameFit = ame_repL(
	Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
	model="bin",symmetric=TRUE,intercept=TRUE,R=latDims,
	nscan=imps, seed=seed, burn=brn, odens=ods,
	plot=FALSE, print=FALSE, gof=TRUE,
	periodicSave=TRUE, outFile=paste0(resultsPath,'ameFit_k0.rda')
	)
