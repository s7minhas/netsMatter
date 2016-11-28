########
# replication of Saleyhan 2008
#######

#
rm(list=ls())


# load libraries
packs = c('dplyr', 'ggplot2', 'ggthemes', 'readr', 'lmtest')

#
source("LoadPkg.R")
source("clusteredSE.R")

#
loadPkg(packs)

# read data and add variables
sal = 
  read_tsv('/Users/juanftellez/Dropbox/netsMatter/replications/saleyhan2008/input data/RefugeesWar_directed.tab') %>% 
  filter(., year >= 1955) %>% 
  mutate(., mccapshare = capshare - .5) %>% 
  mutate(., capref1 = mccapshare*logref1) %>% 
  mutate(., capref2 = mccapshare*logref2)


# base variables
base_vars_mod1 = c('mzinit_lead logref1 logref2 uppcivcon1 uppcivcon2 dem1 dem2 demdem trans1 trans2 transtrans contig colcont capshare s_wt_glo depend1 depend2 igos lpcyrs lpcyrs1 lpcyrs2 lpcyrs3')
base_vars_mod2 = c('mzinit_lead logref1 logref2 capref1 capref2 uppcivcon1 uppcivcon2 dem1 dem2 demdem trans1 trans2 transtrans contig colcont capshare s_wt_glo depend1 depend2 igos lpcyrs lpcyrs1 lpcyrs2 lpcyrs3')


base_vars_mod1 = unlist(strsplit(base_vars_mod1, split = " "))
base_vars_mod2 = unlist(strsplit(base_vars_mod2, split = " "))

form_mod1 = formula(paste0('mzinit_lead ~ ', paste(base_vars_mod1[-1], collapse = '+')))
form_mod2 = formula(paste0('mzinit_lead ~ ', paste(base_vars_mod2[-1], collapse = '+')))

# get rid of missingness
mod1_dat = 
  select(sal, one_of(base_vars_mod1), dyad) %>% 
  filter(complete.cases(.))

mod2_dat = 
  select(sal, one_of(base_vars_mod2), dyad) %>% 
  filter(complete.cases(.))


# model 1 probit, no interaction
mod1 = glm(form_mod1, family = binomial(link = 'probit'), data = mod1_dat)
# clustered SEs
base_mod1 = 
  coeftest(mod1, vcov = vcovCluster(mod1, cluster = mod1_dat$dyad))

save(base_mod1, file = '/Users/juanftellez/Dropbox/netsMatter/replications/saleyhan2012/output data/saleyhan_baseModel.rda')

# model 2 probit, interaction
mod2 = glm(form_mod2, family = binomial(link = 'probit'), data = mod2_dat)
# clustered SEs
base_mod2 = coeftest(mod2, vcov = vcovCluster(mod2, cluster = mod2_dat$dyad))

save(base_mod2, file = '/Users/juanftellez/Dropbox/netsMatter/replications/saleyhan2012/output data/saleyhan_baseModelv2.rda')
