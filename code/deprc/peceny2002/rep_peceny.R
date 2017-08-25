## replication Peceny 2002
source("/Users/howardliu/Dropbox/netsRep_Howard_Max/rose2004/clusteredSE.R")
source("/Users/howardliu/Dropbox/netsRep_Howard_Max/rose2004/LoadPkg.R")

packs = c('base','foreign','dplyr', 'ggplot2', 'readr', 'lmtest','multiwayvcov','reshape2')
loadPkg(packs)

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

# load data
data = read.dta("/Users/howardliu/Dropbox/netsMatter/replications/peceny2002/dictators2.dta")

#mid = data$midhost[data$midhost >0]
data$mid = data$midhost >0

mod1_data= select(data, statea, stateb, year, mid, midhost, dictator, personal, military, single, notaiwan, persdem,democ, contig, majpow, ally, loglsrat, advanced, dispute, dispyrs, uspline1, uspline2, uspline3, dspline1, dspline2, dspline3)


# variables
var = c("mid", "dictator",
        'democ', 'contig', 'majpow','ally',' loglsrat', 'advanced', 'dispyrs', 'dspline1', 'dspline2', 'dspline3')
form_mod1 = formula(paste0('mid ~ ', paste(var[-1], collapse = '+')))

mod1 = glm(form_mod1, family = binomial(link = 'logit'), data = mod1_data)
summary(mod1)

#mod2
var = c("mid", "personal", "military", "single",
        'democ', 'contig', 'majpow','ally',' loglsrat', 'advanced', 'dispyrs', 'uspline1', 'uspline2', 'uspline3')
form_mod2 = formula(paste0('mid ~ ', paste(var[-1], collapse = '+')))


mod2 = glm(form_mod2, family = binomial(link = 'logit'), data = mod1_data)
summary(mod2)


#mod3
var = c("mid", "personal", "military", "notaiwan",
        'democ', 'contig', 'majpow','ally',' loglsrat', 'advanced', 'dispyrs', 'uspline1', 'uspline2', 'uspline3')
form_mod3 = formula(paste0('mid3 ~ ', paste(var[-1], collapse = '+')))

mod3 = glm(form_mod3, family = binomial(link = 'logit'), data = mod1_data)
summary(mod3)


#mod4
var = c("mid", "personal", "military", "single", "persdem",
        'democ', 'contig', 'majpow','ally',' loglsrat', 'advanced', 'dispyrs', 'uspline1', 'uspline2', 'uspline3')
form_mod4 = formula(paste0('mid3 ~ ', paste(var[-1], collapse = '+')))

mod4 = glm(form_mod4, family = binomial(link = 'logit'), data = mod1_data)
summary(mod4)
