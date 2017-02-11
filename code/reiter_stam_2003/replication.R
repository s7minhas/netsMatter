########
# replication of Saleyhan 2008
#######

#
rm(list=ls())


# load libraries
packs = c('dplyr', 'ggplot2', 'foreign', 'readr', 'lmtest')

#
source("LoadPkg.R")
source("/Users/juanftellez/OneDrive/netsMatter/code/saleyhan_2008/clusteredSE.R")

#
loadPkg(packs)

# read data and add variables
stam = 
  read.dta('/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/input/doublereduce.dta')

# get variables
base_vars = c('sideaa pdemdtar pdemdin personal military single democ contig majpow ally loglsrat advanced dispyrs dspline1 dspline2 dspline3') %>% 
  strsplit(x = ., split = " ") %>%  unlist()

#
mod_dat = select(stam, one_of(base_vars), idyr, statea, stateb) %>% 
  filter(complete.cases(.))

# formula
form_mod = formula(paste0('sideaa ~ ', paste(base_vars[-1], collapse = '+')))

# model
mod1 = glm(form_mod, family = binomial(link = 'logit'), data = mod_dat)
# clustered SEs
base_mod1 = round(coeftest(mod1, vcov = vcovCluster(mod1, cluster = mod_dat$idyr)), 3)

save(base_mod1, mod1, mod_dat, file = '/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/reiterStam_baseModel.rda')
