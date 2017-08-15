if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
 dPath='~/Dropbox/netsMatter/replications/McDonald_2004/data/'
}


#install.packages('magrittr')

loadPkg=function(toLoad){
    for(lib in toLoad){
            if(!(lib %in% installed.packages()[,1])){ 
              install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
      }
    }

loadPkg('magrittr')

## load data
## Original model:
## load( paste0(dPath,'ameFit_k0.rda') ) ; ameFit_k0=ameFit
## load( paste0(dPath,'ameFit_k1.rda') ) ; ameFit_k1=ameFit
## load( paste0(dPath,'ameFit_k2.rda') ) ; ameFit_k2=ameFit
## load( paste0(dPath,'ameFit_k3.rda') ) ; ameFit_k3=ameFit

## Update on 7/30: Posteriors with intercepts

load( paste0(dPath,'ameFitIntercepts_k0.rda') ) ; ameFit_k0=ameFit
load( paste0(dPath,'ameFitIntercepts_k1.rda') ) ; ameFit_k1=ameFit
load( paste0(dPath,'ameFitIntercepts_k2.rda') ) ; ameFit_k2=ameFit
load( paste0(dPath,'ameFitIntercepts_k3.rda') ) ; ameFit_k3=ameFit
# check out goodness of fit stats

pdf(file=paste0(dPath, "gofPlotK0.pdf"))
amen::gofPlot(ameFit_k0$GOF, symmetric = TRUE)
dev.off()

pdf(file=paste0(dPath, "gofPlotK1.pdf"))
amen::gofPlot(ameFit_k1$GOF, symmetric = TRUE)
dev.off()

pdf(file=paste0(dPath, "gofPlotK2.pdf"))
amen::gofPlot(ameFit_k2$GOF, symmetric = TRUE)
dev.off()
#
pdf(file=paste0(dPath, "gofPlotK3.pdf"))
amen::gofPlot(ameFit_k3$GOF, symmetric = TRUE)
dev.off()

## check out beta params

summStats = function(x){
    res=c(mu=mean(x),
        med=median(x),
        sd=sd(x),
        quantile(x,probs=c(0.025,0.05,0.95,0.975)))
    round(res, 3)
}

apply(ameFit_k0$BETA, 2, summStats) %>% t() %>% head()
apply(ameFit_k1$BETA, 2, summStats) %>% t() %>% head()
apply(ameFit_k2$BETA, 2, summStats) %>% t() %>% head()
apply(ameFit_k3$BETA, 2, summStats) %>% t() %>% head()


apply(ameFit_k0$BETA, 2, summStats) %>% t()


## check out VC params

pdf(file=paste0(dPath, "paramPlotK0.pdf"))
amen::paramPlot(ameFit_k0$VC)
dev.off()

### Beta parameter convergence

dim(ameFit_k0$BETA) #15

pdf(file=paste0(dPath, "BetaplotK0p1.pdf"))
amen::paramPlot(ameFit_k0$BETA[,1:5])
dev.off()


pdf(file=paste0(dPath, "BetaplotK0p2.pdf"))
amen::paramPlot(ameFit_k0$BETA[,6:10])
dev.off()

pdf(file=paste0(dPath, "BetaplotK0p3.pdf"))
amen::paramPlot(ameFit_k0$BETA[,11:15])
dev.off()


## beta convergence plots k=1

pdf(file=paste0(dPath, "BetaplotK1p1.pdf"))
amen::paramPlot(ameFit_k1$BETA[,1:5])
dev.off()


pdf(file=paste0(dPath, "BetaplotK1p2.pdf"))
amen::paramPlot(ameFit_k1$BETA[,6:10])
dev.off()

pdf(file=paste0(dPath, "BetaplotK1p3.pdf"))
amen::paramPlot(ameFit_k1$BETA[,11:15])
dev.off()

## beta convergence plots k=2


pdf(file=paste0(dPath, "BetaplotK2p1.pdf"))
amen::paramPlot(ameFit_k2$BETA[,1:5])
dev.off()


pdf(file=paste0(dPath, "BetaplotK2p2.pdf"))
amen::paramPlot(ameFit_k2$BETA[,6:10])
dev.off()

pdf(file=paste0(dPath, "BetaplotK2p3.pdf"))
amen::paramPlot(ameFit_k2$BETA[,11:15])
dev.off()


## Beta convergence plots k=3

pdf(file=paste0(dPath, "BetaplotK3p1.pdf"))
amen::paramPlot(ameFit_k3$BETA[,1:5])
dev.off()


pdf(file=paste0(dPath, "BetaplotK3p2.pdf"))
amen::paramPlot(ameFit_k3$BETA[,6:10])
dev.off()

pdf(file=paste0(dPath, "BetaplotK3p3.pdf"))
amen::paramPlot(ameFit_k3$BETA[,11:15])
dev.off()
