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

# load data
load( paste0(dPath,'ameFit_k0.rda') ) ; ameFit_k0=ameFit
load( paste0(dPath,'ameFit_k1.rda') ) ; ameFit_k1=ameFit
load( paste0(dPath,'ameFit_k2.rda') ) ; ameFit_k2=ameFit
load( paste0(dPath,'ameFit_k3.rda') ) ; ameFit_k3=ameFit

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

apply(ameFit_k0$BETA, 2, summStats) %>% t() %>% head()
apply(ameFit_k1$BETA, 2, summStats) %>% t() %>% head()
apply(ameFit_k2$BETA, 2, summStats) %>% t() %>% head()
apply(ameFit_k3$BETA, 2, summStats) %>% t() %>% head()


apply(ameFit_k0$BETA, 2, summStats) %>% t()


## check out VC params

pdf(file=paste0(dPath, "paramPlotK0.pdf"))
amen::paramPlot(ameFit_k0$VC)
dev.off()
