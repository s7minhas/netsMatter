## build AME input data for rose 2004
rm(list=ls())


# load pkg
source("clusteredSE.R")
source("LoadPkg.R")
packs = c('foreign','dplyr', 'ggplot2', 'readr', 'lmtest','multiwayvcov')
loadPkg(packs)

# some short-handed functions
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

# load replication data
dataPath = '/Users/howardliu/Dropbox/netsMatter/replications/rose2004/'
data = read.dta(paste0(dataPath,"data4web.dta"))

# get sampling frame
baseVars = c('cty1', 'cty2', 'year', 'pairid', 'ltrade',
        'bothin', 'onein', 'gsp', 'ldist', 'lrgdp', 'lrgdppc', 'regional',
        'custrict',
        'comlang', 'border', 'landl', 'island', 'lareap', 'comcol', 'curcol', 'colony', 'comctry'
        )

dataComp = na.omit(data[, baseVars])
years = sort(unique(dataComp$year)) %>% as.character()
cntriesT = lapply(years, function(t){ char( unique( dataComp$cty1[dataComp$year==t] ) ) }) # to make sure they are even matrices


# DV data
# data = dplyr::select(data, cty1, cty2, year, pairid, ltrade, bothin, onein, gsp, ldist, lrgdp, lrgdppc, regional, custrict, comlang, border, landl, island, lareap, comcol, curcol, colony, comctry, year)
data$year = data$year %>% as.character()

yVar = 'ltrade'

ii = 1

yList = lapply(1:length(years), function(ii){
    slice = data[ which( data$year==years[ii] &
        data$cty1 %in% cntriesT[[ii]] &
        data$cty2 %in% cntriesT[[ii]]
    ), c('cty1', 'cty2', yVar) ]
    head(slice)
    # flip order
    slice_flip = slice
    slice_flip$cty1 = slice$cty2
    slice_flip$cty2 = slice$cty1
    head(slice_flip)
    sliceL = rbind(slice, slice_flip)
    adj = reshape2::acast(sliceL, cty1 ~ cty2, value.var=yVar)
    return( adj[ cntriesT[[ii]], cntriesT[[ii]] ] )
})

# make sure symmetric
yList[[1]]
for(yr in 1:length(yList)){
  print(isSymmetric(yList[[yr]]))
}

k = adj %>% as.data.frame()


# dyadic vars
dVars_mod1 = c(
        'bothin', 'onein', 'gsp', 'ldist', 'lrgdp', 'lrgdppc', 'regional',
        'custrict',
        'comlang', 'border', 'landl', 'island', 'lareap', 'comcol', 'curcol', 'colony', 'comctry')


xDyadList = lapply(1:length(years), function(ii){
	 slice = data[ which(
			data$year==years[ii] &
			data$cty1 %in% cntriesT[[ii]] &
			data$cty2 %in% cntriesT[[ii]]
			), c('cty1', 'cty2', dVars_mod1) ]
    # flip order
    slice_flip = slice
    slice_flip$cty1 = slice$cty2
    slice_flip$cty2 = slice$cty1
    #head(slice_flip)
    sliceL = rbind(slice, slice_flip)
	  sliceL = reshape2::melt(sliceL, id=c('cty1','cty2'))
	  adj = reshape2::acast(sliceL, cty1 ~ cty2 ~ variable, value.var='value')

	  return( adj[ cntriesT[[ii]], cntriesT[[ii]],  ] )
})

# make sure symmetric
yr = 50
for(v in 1:17){
  print( isSymmetric( xDyadList[[yr]][,,v] ) )
}

k = adj[,,10] %>% as.data.frame()


# save dfs
save(yList, xDyadList, file=paste0(dataPath, 'amenData_rose.rda'))
