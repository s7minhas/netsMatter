#############################
# set a path
require(here)
pth = paste0(here::here(), '/replArchive/')
fPth = paste0(pth, '/helpers/')
wPth = paste0(pth, '2_applications/application_data/weeks/')

#
source(paste0(pth, 'helpers/clusteredSE.R'))
library(foreign)
library(lmtest)
library(sandwich)
##############################

##############################
# load data
weeksData = read.dta(paste0(wPth, 'WeeksAPSR2012.dta'))

# subset to relev covars
dv = 'mzinit'
ivs = c(
	'machinejlw_1', 'juntajlw_1', 'bossjlw_1', 'strongmanjlw_1',
	'allotherauts_1', 'newregime_1', 'democracy_2', 'cap_1', 'cap_2',
	'initshare', 'dependlow', 'majmaj', 'minmaj', 'majmin', 'contigdum',
	'logdist', 's_wt_glo', 's_lead_1', 's_lead_2', 'pcyrsmzinit',
	'pcyrsmzinits1', 'pcyrsmzinits2', 'pcyrsmzinits3'
	)
other = 'democracy_1'
ids = c('ccode1', 'ccode2', 'year', 'dirdyadid')

# construct data for modelling
modData = na.omit( weeksData[,c(dv,ivs,other,ids)] )
##############################

##############################
# set up and run glm model
# formula
modForm = formula(
	paste0(dv, '~',
		paste(ivs, collapse=' + ') ) )

# run
mod = glm(modForm, data=modData, family=binomial(link='logit'))

# get standard errors
clust = modData$dirdyadid
clustN = length(unique(clust))
params = length(coef(mod))
u = sandwich::estfun(mod)
uClust = matrix(NA, nrow=clustN, ncol=params)
for(j in 1:params){ uClust[,j]=tapply(u[,j], clust, sum) }
clVcov = vcov(mod) %*% ((clustN/(clustN-1)) * t(uClust) %*% uClust ) %*% vcov(mod)
modSumm = lmtest::coeftest(mod, vcov=clVcov)

# save
save(mod, modSumm, file=paste0(wPth, 'glmFitWeeks.rda'))
##############################
