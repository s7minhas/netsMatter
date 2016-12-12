## build AME input data for peceny 2002

source("/Users/howardliu/netsMatter/code/peceny2002/clusteredSE.R")
source("/Users/howardliu/netsMatter/code/peceny2002/LoadPkg.R")

packs = c('base','foreign','dplyr', 'ggplot2', 'readr', 'lmtest','multiwayvcov','reshape2')
loadPkg(packs)

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }


# load data
dataPath = '/Users/howardliu/Dropbox/netsMatter/replications/peceny2002/'
data = read.dta(paste0(dataPath,"dictators2.dta"))
data$mid = data$midhost >0
data$mid = data$mid %>% as.numeric()

baseVars = c(
  'statea', 'stateb', 'year', 'mid', 'dictator', 'personal', 'military', 'single', 'notaiwan', 'persdem', 'democ', 'contig', 'majpow', 'ally', 'loglsrat', 'advanced', 'dispute', 'dispyrs', 'dspline1', 'dspline2', 'dspline3'
  )

# get sampling frame
dataComp = na.omit(data[, baseVars])
years = sort(unique(dataComp$year)) %>% as.character()

cntriesT = lapply(years, function(t){ char( unique( dataComp$statea[dataComp$year==t] ) ) }) # make sure they are even matrices


# dv (shah's code)
yVar = 'mid'

# Max: fill in missing
yList = lapply(1:length(years), function(ii){
    slice = data[ which(
            data$year==years[ii] &
            data$statea %in% cntriesT[[ii]] &
            data$stateb %in% cntriesT[[ii]]
            ), c('statea', 'stateb', yVar) ]
    missa = unique(slice$statea[!slice$statea %in% slice$stateb])
    missb = unique(slice$stateb[!slice$stateb %in% slice$statea])
    add = cbind(missb, missa, rep(NA, max(length(missa), length(missb)))) # add into slice
    #add
    #names(slice)
    #colnames(add)
    colnames(add) = names(slice)
    slice = rbind(slice, add) # add in
    adj = reshape2::acast(slice, statea ~ stateb, value.var=yVar)
    adj[lower.tri(adj)] <- 0
    return( adj[ cntriesT[[ii]], cntriesT[[ii]] ] ) ## dont work?? maybe NA issues
})


# dyadic vars
dVars_mod1 = c('dictator',
          'democ', 'contig', 'majpow', 'ally', 'loglsrat', 'advanced', 'dispute', 'dispyrs', 'dspline1', 'dspline2', 'dspline3'
)

xDyadList = lapply(1:length(years), function(ii){
	slice = data[ which(
			data$year==years[ii] &
			data$statea %in% cntriesT[[ii]] &
			data$stateb %in% cntriesT[[ii]]
			), c('statea', 'stateb', dVars_mod1) ]
	  #head(slice)
	  slice$statea %>% unique
	  missa = unique(slice$statea[!slice$statea %in% slice$stateb])
    #missa
	  missb = unique(slice$stateb[!slice$stateb %in% slice$statea])
    #missb
    add = matrix(NA, 1, length(dVars_mod1)+2)
	  add[1,1:2] = c(missb, missa)
	  #add
	  colnames(add) = names(slice)
    slice = rbind(slice, add)
    #tail(slice)
	sliceL = reshape2::melt(slice, id=c('statea','stateb'))
	adj = reshape2::acast(sliceL, statea ~ stateb ~ variable, value.var='value')
	adj[lower.tri(adj)] <- 0
	return( adj[ cntriesT[[ii]], cntriesT[[ii]],  ] )
})


# save dfs

save(yList, xDyadList, file=paste0(dataPath, 'amenData_peceny.rda'))
