## replication Rose 2004 WTO --> trade
rm(list=ls())
# load pkg
if(Sys.info()['user']=='howardliu'){
    source("/Users/howardliu/Dropbox/netsMatter/replications/rose2004/clusteredSE.R")
    source("/Users/howardliu/Dropbox/netsMatter/replications/rose2004/LoadPkg.R")

packs = c('foreign','dplyr', 'ggplot2', 'readr', 'lmtest','multiwayvcov')
loadPkg(packs)

# data
data = read.dta("/Users/howardliu/Dropbox/netsMatter/replications/rose2004/data4web.dta")
}


if(Sys.info()['user']=='s7m'){
    source("~/Research/netsMatter/code/helpers/clusteredSE.R")
    source("~/Research/netsMatter/code/helpers/loadPkg.R")

    packs = c('foreign','dplyr', 'ggplot2', 'readr', 'lmtest','multiwayvcov')
    loadPkg(packs)

    # data
    dataPath='~/Dropbox/Research/netsMatter/replications/rose2004/'
    data = read.dta(paste0(dataPath,'data4web.dta'))
}

# mod1
# which var is currency union?
mod1_data= dplyr:: select(data, cty1, cty2, year, pairid, ltrade, bothin, onein, gsp, ldist, lrgdp, lrgdppc, regional, custrict, comlang, border, landl, island, lareap, comcol, curcol, colony, comctry, year)
var = c('ltrade', # log real trade
        'bothin', 'onein', 'gsp', 'ldist', 'lrgdp', 'lrgdppc', 'regional',
        'custrict',
        'comlang', 'border', 'landl', 'island', 'lareap', 'comcol', 'curcol', 'colony', 'comctry', 'as.factor(year)'
        )

form_mod1 = formula(paste0('ltrade ~ ', paste(var[-1], collapse = '+')))

mod1 = glm(form_mod1, family = gaussian, data = mod1_data)
summary(mod1)

# clustered SEs
baseModel <- mod1
baseData <-  mod1_data
baseModelVcov = cluster.vcov(model=baseModel, cluster=baseData$pairid,
    df_correction = FALSE, leverage = 3)
baseModelSumm = coeftest(baseModel, baseModelVcov)
baseModelSumm
save(baseModelSumm, file = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/glmResults_rose.rda")


# mod2 (no ind countries)
# industrial” if they have an IFS country code less than 200
mod2_data = filter(data, cty1 > 200, cty2 >200)

var = c('ltrade', 'bothin', 'onein', 'gsp', 'ldist', 'lrgdp', 'lrgdppc', 'regional',
        'custrict', #currency union?
        'comlang', 'border', 'landl', 'island', 'lareap', 'comcol',
        #'curcol',
        'colony',
        #'comctry',
        'as.factor(year)')

form_mod2 = formula(paste0('ltrade ~ ', paste(var[-1], collapse = '+')))

mod2 = glm(form_mod2, family = gaussian, data = mod2_data)
#summary(mod2)
# clustered SEs
baseModel <- mod2
baseData <-  mod2_data
baseModelVcov = cluster.vcov(model=baseModel, cluster=baseData$pairid,
    df_correction = FALSE, leverage = 3)
baseModelSumm = coeftest(baseModel, baseModelVcov)
baseModelSumm


# mod3 (post1970)
mod3_data = filter(data, year >= 1970)

var = c('ltrade', 'bothin', 'onein', 'gsp', 'ldist', 'lrgdp', 'lrgdppc', 'regional',
        'custrict',
        'comlang', 'border', 'landl', 'island', 'lareap',
        'comcol',
        'curcol', 'colony',
        'comctry',
        'as.factor(year)')

form_mod3 = formula(paste0('ltrade ~ ', paste(var[-1], collapse = '+')))

mod3 = glm(form_mod3, family = gaussian, data = mod3_data)

baseModel <- mod3
baseData <-  mod3_data
baseModelVcov = cluster.vcov(model=baseModel, cluster=baseData$pairid,
    df_correction = FALSE, leverage = 3)
baseModelSumm = coeftest(baseModel, baseModelVcov)
baseModelSumm

# mod4 (country fixed effects) (not working??)
mod4_data = data
var = c('ltrade', 'bothin', 'onein', 'gsp', 'ldist', 'lrgdp', 'lrgdppc', 'regional',
        'custrict',
        'comlang', 'border', 'landl', 'island', 'lareap',
        'comcol',
        'curcol', 'colony',
        'comctry',
        'as.factor(year)',
        'as.factor(pairid)'
        #'as.factor(cty1)', # not sure on this
        #'as.factor(cty2)'
        )

form_mod4 = formula(paste0('ltrade ~ ', paste(var[-1], collapse = '+')))

mod4 = glm(form_mod4, family = gaussian, data = mod4_data) # 1 min to converge

baseModel <- mod4
baseData <-  mod4_data
baseModelVcov = cluster.vcov(model=baseModel, cluster=baseData$pairid,
    df_correction = FALSE, leverage = 3)
baseModelSumm = coeftest(baseModel, baseModelVcov)
baseModelSumm


