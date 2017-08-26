rm(list=ls())

if(Sys.info()['user']=='s7m'){
	dataPath = '~/Dropbox/Research/netsMatter/replications/gibler_2017/'	
	source('~/Research/netsMatter/code/helpers/loadPkg.R')
	source('~/Research/netsMatter/code/helpers/clusteredSE.R')
} else {
	dataPath = '~/Dropbox/netsMatter/replications/gibler_2017/'
	source('netsMatter/code/gibler_2017/loadPkg.R')
}

#
load(paste0(dataPath,"gibler2017_replication.rda"))
data <- gilberData
names(data)[which( names(data) %in% paste0('_spline',1:3) )] = paste0('spline',1:3)

# modspec
# table 6
# column 6
ids = c('ccode1', 'ccode2', 'year', 'dyad')
dv = 'cwmid'
ivs = c('allied','jointdem', 'cwpceyrs_bkt',
	'spline1', 'spline2', 'spline3', 'contig', 'parity4',
	'firstparity', 'riv1' )
data = na.omit( data[,c(ids, dv, ivs)] )

#
modForm = paste(ivs, collapse=' + ') %>% paste0(dv, ' ~ ', .) %>% formula()

# model
mod = glm(modForm, family = binomial(link = 'logit'), data = data)

# clustered SEs
loadPkg('lmtest')
modSumm = coeftest(mod, vcov = vcovCluster(mod, cluster = data$dyad))

#
save(mod, modSumm, file=paste0(dataPath, 'outputData/glmFit.rda'))