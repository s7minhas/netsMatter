## build AME input data for rose 2004

source("/Users/howardliu/Dropbox/netsRep_Howard_Max/rose2004/clusteredSE.R")
source("/Users/howardliu/Dropbox/netsRep_Howard_Max/rose2004/LoadPkg.R")

packs = c('base','foreign','dplyr', 'ggplot2', 'readr', 'lmtest','multiwayvcov','reshape2')
loadPkg(packs)

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

dataPath = '/Users/howardliu/Dropbox/netsMatter/replications/rose2004/'
data = read.dta(paste0(dataPath,"data4web.dta"))

# get sampling frame
baseVars = c('cty1', 'cty2', 'year', 'pairid', 'ltrade', # log real trade
        'bothin', 'onein', 'gsp', 'ldist', 'lrgdp', 'lrgdppc', 'regional',
        'custrict',
        'comlang', 'border', 'landl', 'island', 'lareap', 'comcol', 'curcol', 'colony', 'comctry'
        )


dataComp = na.omit(data[, baseVars])
years = sort(unique(dataComp$year)) %>% as.character()
cntriesT = lapply(years, function(t){ char( unique( dataComp$cty1[dataComp$year==t] ) ) }) # make sure they are even matrices


# dv (shah's code)
data = select(data, cty1, cty2, year, pairid, ltrade, bothin, onein, gsp, ldist, lrgdp, lrgdppc, regional, custrict, comlang, border, landl, island, lareap, comcol, curcol, colony, comctry, year)
data$year = data$year %>% as.character()
yVar = 'ltrade'

ii  = 1
yList = lapply(1:length(years), function(ii){
    slice = data[ which(
            data$year==years[ii] &
            data$cty1 %in% cntriesT[[ii]] &
            data$cty2 %in% cntriesT[[ii]]
            ), c('cty1', 'cty2', yVar) ]
    missa = unique(slice$cty1[!slice$cty1 %in% slice$cty2])
    missb = unique(slice$cty2[!slice$cty2 %in% slice$cty1])
    add = cbind(missb, missa, rep(NA, max(length(missa), length(missb)))) # add into slice
    #add
    colnames(add) = names(slice)
    slice = rbind(slice, add) # add in
    adj = reshape2::acast(slice, cty1 ~ cty2, value.var=yVar)
    #adj
    adj[lower.tri(adj)] <- 0
    return( adj[ cntriesT[[ii]], cntriesT[[ii]] ] )
})


# dyadic vars
dVars_mod1 = c(
        'bothin', 'onein', 'gsp', 'ldist', 'lrgdp', 'lrgdppc', 'regional',
        'custrict',
        'comlang', 'border', 'landl', 'island', 'lareap', 'comcol', 'curcol', 'colony', 'comctry'
        )

xDyadList = lapply(1:length(years), function(ii){
	slice = data[ which(
			data$year==years[ii] &
			data$cty1 %in% cntriesT[[ii]] &
			data$cty2 %in% cntriesT[[ii]]
			), c('cty1', 'cty2', dVars_mod1) ]
	  #head(slice)
	  missa = unique(slice$cty1[!slice$cty1 %in% slice$cty2])
    #missa
	  missb = unique(slice$cty2[!slice$cty2 %in% slice$cty1])
    #missb
	  add = cbind(missb, missa)
    #add
	  add_cov = matrix(NA, max(length(missa), length(missb)), length(dVars_mod1))
	  #add_cov
	  add = cbind(add,add_cov)
	  #add
	  colnames(add) = names(slice)
    slice = rbind(slice, add)
	sliceL = reshape2::melt(slice, id=c('cty1','cty2'))
	adj = reshape2::acast(sliceL, cty1 ~ cty2 ~ variable, value.var='value')
	# adj[lower.tri(adj)] <- 0 # this seems wrong, try the below, sm
    for(p in 1:dim(adj)[3]){ x=adj[,,p] ; x[lower.tri(x)] = x[upper.tri(x)] ; adj[,,p]=x ; rm(x) }
	return( adj[ cntriesT[[ii]], cntriesT[[ii]],  ] )
})


# save dfs
save(yList, xDyadList, file=paste0(dataPath, 'amenData_rose.rda'))

