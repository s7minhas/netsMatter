rm(list=ls())
## Script to replicate the results in McDonald (2004)

## paths
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
 dataPath='~/Dropbox/netsMatter/replications/McDonald_2004/data/'
}

if(Sys.info()['user']=='s7m'){
 dataPath='~/Dropbox/Research/netsMatter/replications/McDonald_2004/data/'
}

# load libraries
## install/load libraries

loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
	}
    }

loadPkg(c('foreign', 'lmtest', 'sandwich'))

## load data
McDdata = foreign::read.dta(paste0(dataPath, 'PTTOFTfvs.dta'))
## note that appears to have all of the variables that he created in the .do file.


dim(McDdata) ## 534274 x 23

length(is.na(McDdata)) ##lots of NA values

## DV:
table(McDdata$cw2mid)

##################################
##REGRESSIONS
#################################

########################
##Table 1, Model 1
#####################

## stata call:

##. logit cw2mid cw2midspl cw2midsp1 cw2midsp2 cw2midsp3 ally cont1 lncaprat  ldep2l
##grow61l lpolity42l s_wt_glo  lrgdpch61h lndistan majpow limpduty0200h, cluster(dyadid)

dv <- 'cw2mid'

## Table 1, model 1
ivs <- c('cw2midspl','cw2midsp1','cw2midsp2', 'cw2midsp3',
         'ally','cont1', 'lncaprat', 'ldep2l',
         'grow61l', 'lpolity42l','s_wt_glo', 'lrgdpch61h', 'lndistan',
         'majpow', 'limpduty0200h')


colnames(McDdata)

## Table 2, model 1 (interactions)
ivs2 <- c('ldep2impduty')

ids = c('ccode1', 'ccode2', 'year', 'dyadid')

## list of all of the columns that I need to keep:
## construct data for modelling

modData = na.omit(McDdata[,c(dv, ivs, ids)]) #dirty, but almost def
                                        #what STATA did

dim(modData)

head(modData)

## run glm

modForm = formula(
	paste0(dv, '~', 
		paste(ivs, collapse=' + ')
		)
	)
mod = glm(modForm, data=modData, family=binomial(link='logit'))


print("GLM Model estimated")

## none of the pkgs working so going manual
## code from: s7m
clust = modData$dyadid
clustN = length(unique(clust))
params = length(coef(mod))
u = sandwich::estfun(mod)
uClust = matrix(NA, nrow=clustN, ncol=params)
for(j in 1:params){ uClust[,j]=tapply(u[,j], clust, sum) }
clVcov = vcov(mod) %*% ((clustN/(clustN-1)) * t(uClust) %*% uClust ) %*% vcov(mod)

## feed revised vcov in
## results match with stata
modSumm = lmtest::coeftest(mod, vcov=clVcov)

proMod = glm(modForm, data=modData, family=binomial(link='probit'))
clust = modData$dyadid
clustN = length(unique(clust))
params = length(coef(proMod))
u = sandwich::estfun(proMod)
uClust = matrix(NA, nrow=clustN, ncol=params)
for(j in 1:params){ uClust[,j]=tapply(u[,j], clust, sum) }
clVcov = vcov(proMod) %*% ((clustN/(clustN-1)) * t(uClust) %*% uClust ) %*% vcov(proMod)
proSumm = lmtest::coeftest(proMod, vcov=clVcov)


print("saving replicated results")

##  save replicated results
## for the first model (though not the interaction model)

save(modSumm, file=paste0(dataPath, 'McDonald_baseModel.rda'))

save(mod, file=paste0(dataPath, "McDonald_baseModelGLMObj.rda"))

save(mod, modSumm, proMod, proSumm, file=paste0(dataPath, 'mcdonald_glmfit.rda'))

print("completed!")
