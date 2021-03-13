#############################
# set a path
require(here)
pth = paste0(here::here(), '/replArchive/')
rsPth = paste0(pth, '2_applications/application_data/reiter_stam/')

#
source(paste0(pth, 'helpers/clusteredSE.R'))
library(dplyr)
library(lmtest)
library(foreign)
##############################

##############################
# load data
stam = read.dta(paste0(rsPth, 'doublereduce.dta'))

# get variables
base_vars = c('sideaa pdemdtar pdemdin personal military single democ contig majpow ally loglsrat advanced dispyrs dspline1 dspline2 dspline3') %>%
  strsplit(x = ., split = " ") %>%  unlist()

# get model data
mod_dat = select(stam, one_of(base_vars), idyr, statea, stateb) %>%
  filter(complete.cases(.))
##############################

##############################
# set up and run glm model
# formula
form_mod = formula(paste0('sideaa ~ ', paste(base_vars[-1], collapse = '+')))

mod = glm(form_mod, family = binomial(link = 'logit'), data = mod_dat)
modSumm = coeftest(mod, vcov = vcovCluster(mod, cluster = mod_dat$idyr))

# save
save(mod, modSumm, file=paste0(rsPth, 'glmFitReiterStam.rda'))
##############################
