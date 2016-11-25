rm(list=ls())
library(devtools) ; devtools::install_github('s7minhas/amen')
library(amen)

##############################
# load some other data
data(coldwar)

# code modified from hoff vignette pg 43-44
Y=sign( coldwar$cc ) ; n=nrow(Y) ; t=dim(Y)[3]
Y[Y==1] = 0  ; Y[Y==-1] = 1
unique(c(Y))
# nodal covariates
Xn = array(NA,dim=c(n,2,t),dimnames=list(rownames(Y),c('lgdp','polity'),NULL))
for(x in 1:t){ Xn[,'lgdp',x] = log(coldwar$gdp[,x]) ; Xn[,'polity',x] = coldwar$polity[,x] }
# dyadic covariates
Xd=array(dim=c(nrow(Y),nrow(Y),1,t),dimnames=list(rownames(Y),rownames(Y),'ldist',NULL))
Xd[,,'ldist',] = log(coldwar$distance)

# change actor composition over time
degYr = apply(Y, c(1,3), sum, na.rm=TRUE)
noInt = names(rowSums(degYr)[rowSums(degYr)==0])
noIntLate = names(rowSums(degYr[,5:8])[rowSums(degYr[,5:8])==0])
noIntEarly = names(rowSums(degYr[,1:4])[rowSums(degYr[,1:4])==0])
noIntLate = setdiff(noIntLate, noInt)
noIntEarly = setdiff(noIntEarly, noInt)

actors = rownames(Y)
noIntNum = which(actors %in% noInt)
noIntLateNum = which(actors %in% noIntLate)
noIntEarlyNum = which(actors %in% noIntEarly)

# modify Y matrix
yL = lapply(1:t, function(x){ Y[,,x] }) ; names(yL) = dimnames(Y)[[3]]
yL[[1]] = yL[[1]][ actors[-c(noIntNum,noIntEarlyNum[c(3,6,9)])], actors[-c(noIntNum,noIntEarlyNum[c(3,6,9)])] ]
yL[[2]] = yL[[2]][ actors[-c(noIntNum,noIntEarlyNum[c(3,6)])], actors[-c(noIntNum,noIntEarlyNum[c(3,6)])]  ]
yL[[3]] = yL[[3]][ actors[-c(noIntNum,noIntEarlyNum[c(3)])], actors[-c(noIntNum,noIntEarlyNum[c(3)])]  ]
yL[[8]] = yL[[8]][ actors[-c(noIntLateNum[c(3,4)])], actors[-c(noIntLateNum[c(3,4)])]  ]

# modify x node matrix
xNodeL = lapply(1:t, function(x){ Xn[,,x] }) ; names(xNodeL) = names(yL)
xNodeL[[1]] = xNodeL[[1]][ actors[-c(noIntNum,noIntEarlyNum[c(3,6,9)])], ]
xNodeL[[2]] = xNodeL[[2]][ actors[-c(noIntNum,noIntEarlyNum[c(3,6)])],  ]
xNodeL[[3]] = xNodeL[[3]][ actors[-c(noIntNum,noIntEarlyNum[c(3)])],  ]
xNodeL[[8]] = xNodeL[[8]][ actors[-c(noIntLateNum[c(3,4)])],  ]


# modify x dyad matrix
xDyadL = lapply(1:t, function(x){ array(Xd[,,,x],dim=c(n,n,1),dimnames=list(rownames(Y),rownames(Y),'ldist')) })
xDyadL[[1]] = xDyadL[[1]][ actors[-c(noIntNum,noIntEarlyNum[c(3,6,9)])], actors[-c(noIntNum,noIntEarlyNum[c(3,6,9)])], ]
xDyadL[[2]] = xDyadL[[2]][ actors[-c(noIntNum,noIntEarlyNum[c(3,6)])], actors[-c(noIntNum,noIntEarlyNum[c(3,6)])],  ]
xDyadL[[3]] = xDyadL[[3]][ actors[-c(noIntNum,noIntEarlyNum[c(3)])], actors[-c(noIntNum,noIntEarlyNum[c(3)])],  ]
xDyadL[[8]] = xDyadL[[8]][ actors[-c(noIntLateNum[c(3,4)])], actors[-c(noIntLateNum[c(3,4)])],  ]

xDyadL = lapply(1:length(xDyadL), 
	function(t){ array(xDyadL[[t]], 
		dim=c(nrow(yL[[t]]),nrow(yL[[t]]),1), 
		dimnames=list(rownames(yL[[t]]),rownames(yL[[t]]),'ldist') ) 
})
names(xDyadL) = names(yL)

# Save data
save(yL, xNodeL, xDyadL, file='~/Dropbox/Research/netsMatter/replications/example/inputData/exampleData.rda')
##############################

##############################
# ordinal ame_rep with symmetric=TRUE, R=1
# fit=ame_repL(Y=yL,Xdyad=xDyadL,Xrow=xNodeL,Xcol=NULL, R=2,
# 	model="bin",symmetric=TRUE,
# 	burn=500,nscan=1000,odens=10,plot=FALSE, print=FALSE, seed=6886)
# save(fit, file='~/Dropbox/Research/netsMatter/replications/example/outputData/model_k2.rda')
##############################

##############################
# run ame in parallel
# mcmc params
imps = 100000
brn = 50000
ods = 10
latDims = 1:4
seed=6886

# Run amen in parallel
library(doParallel) ; library(foreach)
cl=makeCluster(4) ; registerDoParallel(cl)
foreach(ii=1:length(latDims), .packages=c("amen")) %dopar% {
	
	ameFit = ame_repL(
		Y=yL,Xdyad=xDyadL,Xrow=xNodeL,Xcol=NULL, model="bin",symmetric=TRUE,
		R=latDims[ii], 
		nscan=imps, seed=seed, burn=brn, odens=ods, 
		plot=FALSE, print=FALSE) 
	
	save(ameFit, file='~/Dropbox/Research/netsMatter/replications/example/outputData/model_k',latDims[ii],'.rda')
}
stopCluster(cl)
##############################

##############################
# Assess convergence
load('~/Dropbox/Research/netsMatter/replications/example/outputData/model_k2.rda')

library(reshape2)
library(ggplot2)
beta = data.frame(fit$BETA)
names(beta)[ncol(beta)] = 'distance'
beta$iter = 1:nrow(beta)*10
ggBeta = reshape2::melt(beta,id='iter')
ggplot(ggBeta, aes(x=iter,y=value)) + geom_line() + facet_wrap(~variable,nrow=2)
##############################

