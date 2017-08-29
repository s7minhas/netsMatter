### build AME input data for gilber 2017
rm(list=ls())

# packages
source("/Users/howardliu/netsMatter/code/gilber_2017/LoadPkg.R")
packs = c('base','foreign','dplyr','reshape2',
          'readstata13', 'stringr', 'magrittr')
loadPkg(packs)

# load dta data
dataPath = '/Users/howardliu/Dropbox/netsMatter/replications/gibler_2017/'
load(paste0(dataPath,"gibler2017_replication.rda"))
data <- gilberData

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

# get sampling frame
baseVars = c('ccode1', 'ccode2', 'year', 'dyad',
             'cwmid',
             'allied','jointdem', 'cwpceyrs_bkt', '_spline1', '_spline2', '_spline3', 'contig', 'parity4',
             'firstparity', 'riv1'
)

## trim the ccode
data$ccode1 = data$ccode1 %>% str_replace_all(. , "-.*", "") %>% str_replace_all(., " ", "")
data$ccode2 = data$ccode2 %>% str_replace_all(. , "-.*", "") %>% str_replace_all(., " ", "")

dataComp = na.omit(data[, baseVars])
years = sort(unique(dataComp$year)) %>% as.character()
cntriesT = lapply(years, function(t){ char( unique( dataComp$ccode1[dataComp$year==t] ) ) }) # make sure they are even matrices
cntriesT %>% length
cntriesT %>% head

#### dv (shah's code)
data = dplyr::select(data, ccode1, ccode2, year, dyad,
                     cwmid, allied, jointdem, cwpceyrs_bkt, `_spline1`, `_spline2` ,`_spline3`, contig, parity4,
                     firstparity, riv1)

data$year = data$year %>% as.character()
yVar = 'cwmid'

yList = lapply(1:length(years), function(ii){
  slice = data[ which(
    data$year==years[ii] &
      data$ccode1 %in% cntriesT[[ii]] &
      data$ccode2 %in% cntriesT[[ii]]
  ), c('ccode1', 'ccode2', yVar) ]
  missa = unique(slice$ccode1[!slice$ccode1 %in% slice$ccode2])
  missb = unique(slice$ccode2[!slice$ccode2 %in% slice$ccode1])
  add = cbind(missb, missa, rep(NA, max(length(missa), length(missb)))) # add into slice
  add
  colnames(add) = names(slice)
  slice = rbind(slice, add) # add in
  slice$cwmid = as.numeric(slice$cwmid)
  adj = reshape2::acast(slice, ccode1 ~ ccode2, value.var=yVar)
  #adj
  # convert to sym matrix
  adj[lower.tri(adj)] = 0
  adj = adj + t(adj)
  return( adj[ cntriesT[[ii]], cntriesT[[ii]] ] )
})

# check if symmetric
dv = yList[[1]] %>% as.matrix()
isSymmetric(dv)

####### dyadic vars
dVars_mod1 = c(
  'allied','jointdem', 'cwpceyrs_bkt', '_spline1', '_spline2', '_spline3', 'contig', 'parity4',
  'firstparity', 'riv1'
)

ii = 10
xDyadList = lapply(1:length(years), function(ii){
     slice = data[ which(
         data$year==years[ii] &
             data$ccode1 %in% cntriesT[[ii]] &
             data$ccode2 %in% cntriesT[[ii]]
       ), c('ccode1', 'ccode2', dVars_mod1) ]
    missa = unique(slice$ccode1[!slice$ccode1 %in% slice$ccode2])
    missb = unique(slice$ccode2[!slice$ccode2 %in% slice$ccode1])
    add = cbind(missb, missa)
    add_cov = matrix(NA, max(length(missa), length(missb)), length(dVars_mod1))
    add = cbind(add,add_cov)
    colnames(add) = names(slice)
    slice = rbind(slice, add)
    # # convert to numerics
    # slice$allied = as.numeric(slice$allied)
    # slice$jointdem = as.numeric(slice$jointdem)
    # slice$cwpceyrs_bkt = as.numeric(slice$cwpceyrs_bkt)
    # slice$`_spline1` = as.numeric(slice$`_spline1`)
    # slice$`_spline2` = as.numeric(slice$`_spline2`)
    # slice$`_spline3` = as.numeric(slice$`_spline3`)
    # slice$contig = as.numeric(slice$contig)
    # slice$parity4 = as.numeric(slice$parity4)

    sliceL = reshape2::melt(slice, id=c('ccode1','ccode2'))
    sliceL$value = as.numeric(sliceL$value)
    adj = reshape2::acast(sliceL, ccode1 ~ ccode2 ~ variable, value.var='value')

    # convert to symmetric
    for(p in 1:dim(adj)[3]){ x=adj[,,p] ; x[lower.tri(x)] = x[upper.tri(x)] ; x = x + t(x);  adj[,,p]=x ; rm(x) }

    return( adj[ cntriesT[[ii]], cntriesT[[ii]],  ] )
})

xDyadList[[1]][,,2]
idv = xDyadList[[1]][,,1]  %>% as.matrix()
isSymmetric(idv)

# save dfs
save(yList, xDyadList, file=paste0(dataPath, 'amenData_gibler2.rda')) # add in firstparity and riv1
