
##### Wright and Diehl 2014 AMEN "As-Is" Replication #####



rm(list=ls())

# devtools::install_github('s7minhas/amen')
# use this to install AMEN if needed
library(amen)
library(foreign)
library(lmtest)
library(multiwayvcov)

if(Sys.info()['user']=='jordan'){
  load("/Users/jordan/Dropbox/netsMatter/replications/wrightdiehl/wdData.R")
}

# if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
#   data = read.dta("~/Dropbox/research/netsMatter/replications/gartzke2007/capitalistpeace_012007.dta")
# }




##########################################
####### Build AMEN format data
##########################################

# get "both sides" of the undirected data
data2 = data
data2$statea = data$stateb
data2$stateb = data$statea
data = rbind(data, data2)

# NOTE: this section adapted from Minhas code

char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

# load data
baseVars = c('maoznewl', 'demlo', 'demhi', 'deplo',
             'capopenl', 'rgdppclo', 'gdpcontg',
             'sun2cati', 'contig', 'logdstab',
             'majpdyds', 'alliesr', 'lncaprt',
             'spline1', 'spline2', 'spline3', 
             'year', 'statea', 'stateb'
)

# get sampling frame
dataComp = na.omit(data[, baseVars])
yrs = sort(unique(dataComp$year)) 
cntriesT = lapply(yrs, function(t){ char( unique( dataComp$statea[dataComp$year==t] ) ) })
#cntriesTb = lapply(yrs, function(t){ char( unique( dataComp$stateb[dataComp$year==t] ) ) })

# dv
yVar = 'maoznewl'
yList = lapply(1:length(yrs), function(ii){
  slice = data[ which( 
    data$year==yrs[ii] & 
      data$statea %in% cntriesT[[ii]] & 
      data$stateb %in% cntriesT[[ii]]
  ), c('statea', 'stateb', yVar) ]
  adj = reshape2::acast(slice, statea ~ stateb, value.var=yVar)
  return( adj[ cntriesT[[ii]], cntriesT[[ii]] ] )
})

# dyadic vars
dVars = c('demlo', 'demhi', 'deplo',
          'capopenl', 'rgdppclo', 'gdpcontg',
          'sun2cati', 'contig', 'logdstab',
          'majpdyds', 'alliesr', 'lncaprt',
          'spline1', 'spline2', 'spline3')
xDyadList = lapply(1:length(yrs), function(ii){
  slice = data[ which( 
    data$year==yrs[ii] & 
      data$statea %in% cntriesT[[ii]] & 
      data$stateb %in% cntriesT[[ii]]
  ), c('statea', 'stateb', dVars) ]
  sliceL = reshape2::melt(slice, id=c('statea','stateb'))
  adj = reshape2::acast(sliceL, statea ~ stateb ~ variable, value.var='value')
  return( adj[ cntriesT[[ii]], cntriesT[[ii]],  ] )
})



# quick check
# xDyadList[[1]][, , 1]
# xDyadList[[1]][, , 2]




### No nodal vars in this paper
# # nodal vars
# nVars = c(
#   ''
# )
# xNodeList = lapply(1:length(yrs), function(ii){
#   slice = unique( data[ which( 
#     data$year==yrs[ii] & 
#       data$ccode1 %in% cntriesT[[ii]] & 
#       data$ccode2 %in% cntriesT[[ii]]
#   ), c('ccode1', nVars) ] )
#   if(nrow(slice)!=length(cntriesT[[ii]])){ stop('# rows dont match')  }
#   regionSplit = model.matrix(~region-1, data=slice)
#   adj = data.matrix(cbind( slice[,nVars[-length(nVars)]], regionSplit ))
#   rownames(adj) = slice$ccode1
#   return( adj[ cntriesT[[ii]], ]  )
# })

# save dfs
save(yList, xDyadList, #xNodeList, 
     file='/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/Replication\ specific\ data/gartzke2007amenData.rda')
