########
# replication of Reiter and Stam 2003 results
#######

#
rm(list=ls())


# load libraries
packs = c('dplyr', 'ggplot2', 'foreign', 'readr', 'lmtest')

#
if(Sys.info()['user']=='juanftellez'){
	source("LoadPkg.R")
	source("/Users/juanftellez/OneDrive/netsMatter/code/helpers/clusteredSE.R")

	#
	loadPkg(packs)

	# read data and add variables
	stam = 
	  read.dta('/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/input/doublereduce.dta')
}

if(Sys.info()['user']=='s7m'){
	source("~/Research/netsMatter/code/helpers/loadPkg.R")
	source("~/Research/netsMatter/code/helpers/clusteredSE.R")

	#
	loadPkg(packs)

	# read data and add variables
	dataPath = '~/Dropbox/Research/netsMatter/replications/Reiter_Stam_2003/input/'
	resultsPath = '~/Dropbox/Research/netsMatter/replications/Reiter_Stam_2003/output/'
	stam = read.dta(paste0(dataPath, 'doublereduce.dta'))
}

# get variables
base_vars = c('sideaa pdemdtar pdemdin personal military single democ contig majpow ally loglsrat advanced dispyrs dspline1 dspline2 dspline3') %>% 
  strsplit(x = ., split = " ") %>%  unlist()

# get model data
mod_dat = select(stam, one_of(base_vars), idyr, statea, stateb) %>% 
  filter(complete.cases(.))

# formula
form_mod = formula(paste0('sideaa ~ ', paste(base_vars[-1], collapse = '+')))

# model
if(Sys.info()['user']=='juanftellez'){
mod1 = glm(form_mod, family = binomial(link = 'logit'), data = mod_dat)
# clustered SEs
base_mod1 = round(coeftest(mod1, vcov = vcovCluster(mod1, cluster = mod_dat$idyr)), 3)

save(base_mod1, mod1, mod_dat, file = '/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/reiterStam_baseModel.rda')
}

if(Sys.info()['user']=='s7m'){
	mod = glm(form_mod, family = binomial(link = 'logit'), data = mod_dat)
	modSumm = coeftest(mod, vcov = vcovCluster(mod, cluster = mod_dat$idyr))

	proMod = glm(form_mod, family = binomial(link = 'probit'), data = mod_dat)
	proSumm = coeftest(proMod, vcov = vcovCluster(proMod, cluster = mod_dat$idyr))

	save(mod, modSumm, proMod, proSumm, file=paste0(resultsPath, 'reiter_stam_glmfit.rda'))
}