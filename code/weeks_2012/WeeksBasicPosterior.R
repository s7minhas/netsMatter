if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
 dPath='~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'}


loadPkg=function(toLoad){
    for(lib in toLoad){
            if(!(lib %in% installed.packages()[,1])){ 
              install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
      }
    }

loadPkg('magrittr')

# load data
load( paste0(dPath,'model_k02017-03-15_v2.rda') ) ; ameFit_k0=ameFit
load( paste0(dPath,'model_k12017-03-15_v2.rda') ) ; ameFit_k1=ameFit
#load( paste0(dPath,'') ) ; ameFit_k2=ameFit
load( paste0(dPath,'model_k32017-03-14_v2.rda') ) ; ameFit_k3=ameFit

# check out goodness of fit stats

pdf(file=paste0(dPath, "gofPlotK0.pdf"))
amen::gofPlot(ameFit_k0$GOF, symmetric = TRUE)
dev.off()

pdf(file=paste0(dPath, "gofPlotK1.pdf"))
amen::gofPlot(ameFit_k1$GOF, symmetric = TRUE)
dev.off()

## pdf(file=paste0(dPath, "gofPlotK2.pdf"))
## amen::gofPlot(ameFit_k2$GOF, symmetric = TRUE)
## dev.off()

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
#apply(ameFit_k2$BETA, 2, summStats) %>% t() %>% head()
apply(ameFit_k3$BETA, 2, summStats) %>% t() %>% head()


apply(ameFit_k0$BETA, 2, summStats) %>% t()


## check out VC params

pdf(file=paste0(dPath, "paramPlotK0.pdf"))
amen::paramPlot(ameFit_k0$VC)
dev.off()

pdf(file=paste0(dPath, "paramPlotK3.pdf"))
amen::paramPlot(ameFit_k3$VC)
dev.off()

pdf(file=paste0(dPath, "paramPlotK1.pdf"))
amen::paramPlot(ameFit_k1$VC)
dev.off()


## Beta stability
## nead to break up the beta coef lists:


pdf(file=paste0(dPath, "BetaplotK0p1.pdf"))
amen::paramPlot(ameFit_k0$BETA[,1:5])
dev.off()


pdf(file=paste0(dPath, "BetaplotK0p2.pdf"))
amen::paramPlot(ameFit_k0$BETA[,6:10])
dev.off()

pdf(file=paste0(dPath, "BetaplotK0p3.pdf"))
amen::paramPlot(ameFit_k0$BETA[,11:15])
dev.off()


pdf(file=paste0(dPath, "BetaplotK0p4.pdf"))
amen::paramPlot(ameFit_k0$BETA[,16:20])
dev.off()


pdf(file=paste0(dPath, "BetaplotK0p5.pdf"))
amen::paramPlot(ameFit_k0$BETA[,21:25])
dev.off()


pdf(file=paste0(dPath, "BetaplotK0p6.pdf"))
amen::paramPlot(ameFit_k0$BETA[,25:28])
dev.off()

####
##Beta plots for lat dims=1

pdf(file=paste0(dPath, "BetaplotK1.pdf"))
amen::paramPlot(ameFit_k1$BETA)
dev.off()

pdf(file=paste0(dPath, "BetaplotK1p1.pdf"))
amen::paramPlot(ameFit_k1$BETA[,1:5])
dev.off()


pdf(file=paste0(dPath, "BetaplotK1p2.pdf"))
amen::paramPlot(ameFit_k1$BETA[,6:10])
dev.off()

pdf(file=paste0(dPath, "BetaplotK1p3.pdf"))
amen::paramPlot(ameFit_k1$BETA[,11:15])
dev.off()


pdf(file=paste0(dPath, "BetaplotK1p4.pdf"))
amen::paramPlot(ameFit_k1$BETA[,16:20])
dev.off()


pdf(file=paste0(dPath, "BetaplotK1p5.pdf"))
amen::paramPlot(ameFit_k1$BETA[,21:25])
dev.off()


pdf(file=paste0(dPath, "BetaplotK1p6.pdf"))
amen::paramPlot(ameFit_k1$BETA[,25:28])
dev.off()

## pdf(file=paste0(dPath, "BetaplotK2.pdf"))
## paramPlot(ameFit_k2$BETA)
## dev.off()

#### BETA PLOT K=3

pdf(file=paste0(dPath, "BetaplotK3p1.pdf"))
amen::paramPlot(ameFit_k3$BETA[,1:5])
dev.off()


pdf(file=paste0(dPath, "BetaplotK3p2.pdf"))
amen::paramPlot(ameFit_k3$BETA[,6:10])
dev.off()

pdf(file=paste0(dPath, "BetaplotK3p3.pdf"))
amen::paramPlot(ameFit_k3$BETA[,11:15])
dev.off()


pdf(file=paste0(dPath, "BetaplotK3p4.pdf"))
amen::paramPlot(ameFit_k3$BETA[,16:20])
dev.off()


pdf(file=paste0(dPath, "BetaplotK3p5.pdf"))
amen::paramPlot(ameFit_k3$BETA[,21:25])
dev.off()


pdf(file=paste0(dPath, "BetaplotK3p6.pdf"))
amen::paramPlot(ameFit_k3$BETA[,25:28])
dev.off()
