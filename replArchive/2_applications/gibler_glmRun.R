#############################
# set a path
require(here)
pth = paste0(here::here(), '/replArchive/')
gPth = paste0(pth, '2_applications/application_data/gibler/')

#
source(paste0(pth, 'helpers/clusteredSE.R'))
library(magrittr)
library(lmtest)
##############################

##############################
# load data
load(paste0(gPth,"gibler2017_replication.rda"))
data <- gilberData
names(data)[which( names(data) %in% paste0('_spline',1:3) )] = paste0('spline',1:3)
##############################

##############################
# set up and run glm model
# modspec # table 6 # column 6
ids = c('ccode1', 'ccode2', 'year', 'dyad')
dv = 'cwmid'
ivs = c('allied','jointdem', 'cwpceyrs_bkt',
	'spline1', 'spline2', 'spline3', 'contig', 'parity4',
	'firstparity', 'riv1' )
data = na.omit( data[,c(ids, dv, ivs)] )

# set up model formula
modForm = paste(ivs, collapse=' + ') %>% paste0(dv, ' ~ ', .) %>% formula()

# run
mod = glm(modForm, family = binomial(link = 'logit'), data = data)

# get clustered SEs
modSumm = coeftest(mod, vcov = vcovCluster(mod, cluster = data$dyad))

# save
save(mod, modSumm, file=paste0(gPth, 'glmFitGibler.rda'))
##############################
