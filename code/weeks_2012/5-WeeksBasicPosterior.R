
##This file takes Ame output and generates goodness of fit and beta distribution plots
## to diagnose convergence

if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
 dPath='~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'}

if(Sys.info()['user']== 'margaret'){
    source('~/projects/netsmatter/code/netsMatter/code/weeks_2012/setup.R')
}


loadPkg=function(toLoad){
    for(lib in toLoad){
            if(!(lib %in% installed.packages()[,1])){ 
              install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
      }
    }

loadPkg('magrittr')

# load data
load( paste0(dPath,'model_k0.rda') ) ; ameFit_k0=ameFit
load( paste0(dPath,'model_k1.rda') ) ; ameFit_k1=ameFit
load( paste0(dPath,'model_k2.rda') ) ; ameFit_k2=ameFit
load( paste0(dPath,'model_k3.rda') ) ; ameFit_k3=ameFit


## attributes

attributes(ameFit_k0)

colnames(ameFit_k0$BETA)

length(unique(colnames(ameFit_k0$BETA)))

head(ameFit_k0$BETA)

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

## sending these into a csv file
write.csv(apply(ameFit_k0$BETA, 2, summStats) %>% t() %>% head(), file=paste0(dPath, "resultsTabK0.csv"))
apply(ameFit_k1$BETA, 2, summStats) %>% t() %>% head()
apply(ameFit_k2$BETA, 2, summStats) %>% t() %>% head()
apply(ameFit_k3$BETA, 2, summStats) %>% t() %>% head()


apply(ameFit_k0$BETA, 2, summStats) %>% t()


## check out VC params


## Date mark for ease of parsing the plots

dat <- as.character(Sys.Date())


pdf(file=paste0(dPath, dat, "paramPlotK0.pdf"))
amen::paramPlot(ameFit_k0$VC)
dev.off()


pdf(file=paste0(dPath, dat, "paramPlotK2.pdf"))
amen::paramPlot(ameFit_k2$VC)
dev.off()

pdf(file=paste0(dPath, dat, "paramPlotK3.pdf"))
amen::paramPlot(ameFit_k3$VC)
dev.off()

pdf(file=paste0(dPath, dat, "paramPlotK1.pdf"))
amen::paramPlot(ameFit_k1$VC)
dev.off()


## Beta stability
## nead to break up the beta coef lists:


pdf(file=paste0(dPath, dat, "BetaplotK0p1.pdf"))
amen::paramPlot(ameFit_k0$BETA[,1:5])
dev.off()


pdf(file=paste0(dPath, dat, "BetaplotK0p2.pdf"))
amen::paramPlot(ameFit_k0$BETA[,6:10])
dev.off()

pdf(file=paste0(dPath, dat, "BetaplotK0p3.pdf"))
amen::paramPlot(ameFit_k0$BETA[,11:15])
dev.off()


pdf(file=paste0(dPath, dat, "BetaplotK0p4.pdf"))
amen::paramPlot(ameFit_k0$BETA[,16:20])
dev.off()


pdf(file=paste0(dPath, dat, "BetaplotK0p5.pdf"))
amen::paramPlot(ameFit_k0$BETA[,21:23])
dev.off()

####
##Beta plots for lat dims=1


pdf(file=paste0(dPath, dat, "BetaplotK1p1.pdf"))
amen::paramPlot(ameFit_k1$BETA[,1:5])
dev.off()


pdf(file=paste0(dPath, dat, "BetaplotK1p2.pdf"))
amen::paramPlot(ameFit_k1$BETA[,6:10])
dev.off()

pdf(file=paste0(dPath, dat, "BetaplotK1p3.pdf"))
amen::paramPlot(ameFit_k1$BETA[,11:15])
dev.off()


pdf(file=paste0(dPath, dat, "BetaplotK1p4.pdf"))
amen::paramPlot(ameFit_k1$BETA[,16:20])
dev.off()


pdf(file=paste0(dPath, dat, "BetaplotK1p5.pdf"))
amen::paramPlot(ameFit_k1$BETA[,21:23])
dev.off()


## Beta Plots for K2


pdf(file=paste0(dPath, dat, "BetaplotK2p1.pdf"))
amen::paramPlot(ameFit_k2$BETA[,1:5])
dev.off()


pdf(file=paste0(dPath, dat, "BetaplotK2p2.pdf"))
amen::paramPlot(ameFit_k2$BETA[,6:10])
dev.off()

pdf(file=paste0(dPath, dat, "BetaplotK2p3.pdf"))
amen::paramPlot(ameFit_k2$BETA[,11:15])
dev.off()


pdf(file=paste0(dPath, dat, "BetaplotK2p4.pdf"))
amen::paramPlot(ameFit_k2$BETA[,16:20])
dev.off()


pdf(file=paste0(dPath, dat, "BetaplotK2p5.pdf"))
amen::paramPlot(ameFit_k2$BETA[,21:23])
dev.off()


#### BETA PLOT K=3

pdf(file=paste0(dPath, dat, "BetaplotK3p1.pdf"))
amen::paramPlot(ameFit_k3$BETA[,1:5])
dev.off()


pdf(file=paste0(dPath, dat, "BetaplotK3p2.pdf"))
amen::paramPlot(ameFit_k3$BETA[,6:10])
dev.off()

pdf(file=paste0(dPath, dat, "BetaplotK3p3.pdf"))
amen::paramPlot(ameFit_k3$BETA[,11:15])
dev.off()


pdf(file=paste0(dPath, dat, "BetaplotK3p4.pdf"))
amen::paramPlot(ameFit_k3$BETA[,16:20])
dev.off()


pdf(file=paste0(dPath, dat, "BetaplotK3p5.pdf"))
amen::paramPlot(ameFit_k3$BETA[,21:23])
dev.off()
