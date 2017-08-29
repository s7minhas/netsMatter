######### run ame model
#library(devtools)
#devtools::install_github('s7minhas/amen')
#if(Sys.info()['user']=='mdw'){
#  load('/Users/mdw/Dropbox/research/Ongoing/netsMatter/replications/gibler_2017/amenData_gibler2.rda' )
#  resultsPath = "/Users/mdw/Dropbox/research/Ongoing//netsMatter/replications/gibler_2017/outputData/"
#}
setwd("~/Desktop/giblerrepl")
rm(list = ls())

load("amenData_gibler2.rda")
library(amen)

str(yList)
yList[1]

# test run 
#brn=200; imps=1000; ods=25

# for real
imps =10000
brn = 500
ods = 50


#### k =2 #####
# params
seed=6886

  ameFit = ame_repL(
    Y=yList,Xdyad=xDyadList,Xrow=NULL,Xcol=NULL,
    model="bin",symmetric=TRUE, # MID DV: undirected data
    R=2,
    nscan=imps, seed=seed, burn=brn, odens=ods,
    plot=FALSE, print=FALSE
  )
  save(ameFit, file='mdwameFit_k2.rda')

  par(mfrow=c(2,3))
  plot(density(ameFit$BETA[,1]))
  plot(density(ameFit$BETA[,2]))
  plot(density(ameFit$BETA[,3]))
  plot(density(ameFit$BETA[,4]))
  plot(density(ameFit$BETA[,8]))
  plot(density(ameFit$BETA[,9]))
  
  summary(ameFit
          )