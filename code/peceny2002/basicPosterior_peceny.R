if(Sys.info()['user']=='howardliu'){
	source('~/netsMatter/code/peceny2002/loadPkg.R') }

#
loadPkg('magrittr')

# load data
resultsPath = "/Users/howardliu/Dropbox/netsMatter/replications/peceny2002/outputData/"
load( paste0(resultsPath,'ameFit_k0_v2_peceny.rda') ) ; ameFit_k0=ameFit
load( paste0(resultsPath,'ameFit_k1_v1_peceny.rda') ) ; ameFit_k1=ameFit
load( paste0(resultsPath,'ameFit_k2_v1.rda') ) ; ameFit_k1=ameFit
load( paste0(resultsPath,'ameFit_k3_v1.rda') ) ; ameFit_k3=ameFit

#load( paste0(resultsPath,'ameFit_k2.rda') ) ; ameFit_k2=ameFit
#load( paste0(resultsPath,'ameFit_k3.rda') ) ; ameFit_k3=ameFit

# check out goodness of fit stats
amen::gofPlot(ameFit_k0$GOF, symmetric = FALSE)
amen::gofPlot(ameFit_k1$GOF, symmetric = FALSE) ## change pix
amen::gofPlot(ameFit_k2$GOF, symmetric = FALSE)
amen::gofPlot(ameFit_k3$GOF, symmetric = FALSE)

# check out beta params
summStats = function(x){
	res=c(mu=mean(x),med=median(x),sd=sd(x),quantile(x,probs=c(0.025,0.05,0.95,0.975)))
	round(res, 3)
}
apply(ameFit_k0$BETA, 2, summStats) %>% t() %>% head()
apply(ameFit_k1$BETA, 2, summStats) %>% t() %>% head()
apply(ameFit_k2$BETA, 2, summStats) %>% t() %>% head()
apply(ameFit_k3$BETA, 2, summStats) %>% t() %>% head()

# lets make some plots, i have a lot of params so i need to space stuff out
betaIndices<-split(1:ncol(ameFit_k0$BETA), ceiling(seq_along(1:ncol(ameFit_k0$BETA))/5))
betaIndices<-split(1:ncol(ameFit_k1$BETA), ceiling(seq_along(1:ncol(ameFit_k1$BETA))/5))
betaIndices<-split(1:ncol(ameFit_k2$BETA), ceiling(seq_along(1:ncol(ameFit_k2$BETA))/5))
betaIndices<-split(1:ncol(ameFit_k3$BETA), ceiling(seq_along(1:ncol(ameFit_k3$BETA))/5))

#k=0
for(bIndex in betaIndices){ amen::paramPlot( ameFit_k0$BETA[,bIndex,drop=FALSE] ) }
for(bIndex in betaIndices){ amen::paramPlot( ameFit_k1$BETA[,bIndex,drop=FALSE] ) }
for(bIndex in betaIndices){ amen::paramPlot( ameFit_k2$BETA[,bIndex,drop=FALSE] ) }
for(bIndex in betaIndices){ amen::paramPlot( ameFit_k3$BETA[,bIndex,drop=FALSE] ) }

# check out VC params
amen::paramPlot(ameFit_k0$VC)
amen::paramPlot(ameFit_k1$VC)
amen::paramPlot(ameFit_k2$VC)
amen::paramPlot(ameFit_k3$VC)

latDims = 0
pdf(paste0(resultsPath, "BETAPlot_k", latDims,"_v2", ".pdf"))
for(bIndex in betaIndices){ amen::paramPlot( ameFit_k0$BETA[,bIndex,drop=FALSE] ) }
dev.off()

pdf(paste0(resultsPath, "paramPlot_k", latDims,"_v2", ".pdf"))
amen::paramPlot(ameFit_k0$VC)
dev.off()

pdf(paste0(resultsPath, "GOFPlot_k", latDims,"_v2", ".pdf"))
amen::gofPlot(ameFit_k0$GOF, symmetric = FALSE)
dev.off()

######
latDims = 1
pdf(paste0(resultsPath, "BETAPlot_k", latDims, ".pdf"))
for(bIndex in betaIndices){ amen::paramPlot( ameFit_k1$BETA[,bIndex,drop=FALSE] ) }
dev.off()

pdf(paste0(resultsPath, "paramPlot_k", latDims, ".pdf"))
amen::paramPlot(ameFit_k1$VC)
dev.off()

pdf(paste0(resultsPath, "GOFPlot_k", latDims, ".pdf"))
amen::gofPlot(ameFit_k1$GOF, symmetric = FALSE)
dev.off()

######
latDims = 2
pdf(paste0(resultsPath, "BETAPlot_k", latDims, ".pdf"))
for(bIndex in betaIndices){ amen::paramPlot( ameFit_k2$BETA[,bIndex,drop=FALSE] ) }
dev.off()

pdf(paste0(resultsPath, "paramPlot_k", latDims, ".pdf"))
amen::paramPlot(ameFit_k2$VC)
dev.off()

pdf(paste0(resultsPath, "GOFPlot_k", latDims, ".pdf"))
amen::gofPlot(ameFit_k2$GOF, symmetric = FALSE)
dev.off()


######
latDims = 3
pdf(paste0(resultsPath, "BETAPlot_k", latDims, ".pdf"))
for(bIndex in betaIndices){ amen::paramPlot( ameFit_k3$BETA[,bIndex,drop=FALSE] ) }
dev.off()

pdf(paste0(resultsPath, "paramPlot_k", latDims, ".pdf"))
amen::paramPlot(ameFit_k3$VC)
dev.off()

pdf(paste0(resultsPath, "GOFPlot_k", latDims, ".pdf"))
amen::gofPlot(ameFit_k3$GOF, symmetric = FALSE)
dev.off()
