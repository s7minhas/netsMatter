devtools::install_github('s7minhas/amen') ; library(amen)
resultsPath = '/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/'
resultsPath = '~/s7m/Research/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/'
#
library(magrittr)

# load data
load( paste0(resultsPath,'ameFit_k0.rda') ) ; ameFit_k0=ameFit
load( paste0(resultsPath,'ameFit_k1.rda') ) ; ameFit_k1=ameFit
load( paste0(resultsPath,'ameFit_k2.rda') ) ; ameFit_k2=ameFit
load( paste0(resultsPath,'ameFit_k3.rda') ) ; ameFit_k3=ameFit

# check out goodness of fit stats
amen::gofPlot(ameFit_k0$GOF, symmetric = TRUE)
amen::gofPlot(ameFit_k1$GOF, symmetric = TRUE)
amen::gofPlot(ameFit_k2$GOF, symmetric = TRUE)
amen::gofPlot(ameFit_k3$GOF, symmetric = TRUE)

# check out beta params
summStats = function(x){
  res=c(mu=mean(x),med=median(x),sd=sd(x),quantile(x,probs=c(0.025,0.05,0.95,0.975)))
  round(res, 3)
}
apply(ameFit_k0$BETA, 2, summStats) %>% t() %>% nrow()
apply(ameFit_k1$BETA, 2, summStats) %>% t() %>% nrow()
apply(ameFit_k2$BETA, 2, summStats) %>% t() %>% nrow()
apply(ameFit_k3$BETA, 2, summStats) %>% t() %>% nrow()


# lets make some plots, i have a lot of params so i need to space stuff out
betaIndices<-split(1:ncol(ameFit_k0$BETA), ceiling(seq_along(1:ncol(ameFit_k0$BETA))/5))

#k=0
for(bIndex in betaIndices){ amen::paramPlot( ameFit_k0$BETA[,bIndex,drop=FALSE] ) }

# check out VC params
amen::paramPlot(ameFit_k0$VC)
