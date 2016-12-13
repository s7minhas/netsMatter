
##### Gartzke 2007 AMEN "As-Is" Replication #####



rm(list=ls())

# devtools::install_github('s7minhas/amen')
  # use this to install AMEN if needed
library(amen)
library(foreign)
library(lmtest)
library(multiwayvcov)

data = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007.dta")


# rename the spline variables to avoid issues with R syntax
colnames(data)[34] = 'spline1'
colnames(data)[35] = 'spline2'
colnames(data)[36] = 'spline3'



##########################################
####### Build AMEN format data
##########################################

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
      data$ccode1 %in% cntriesT[[ii]] & 
      data$ccode2 %in% cntriesT[[ii]]
  ), c('ccode1', 'ccode2', dVars) ]
  sliceL = reshape2::melt(slice, id=c('ccode1','ccode2'))
  adj = reshape2::acast(sliceL, ccode1 ~ ccode2 ~ variable, value.var='value')
  return( adj[ cntriesT[[ii]], cntriesT[[ii]],  ] )
})

# nodal vars
nVars = c(
  'polity2_04', 'polconiiiA', 'llnGDP_gled08', 'ldGDP_gled08V2', 
  'lheg_new', 'pcw89', 'onsetperc2', 'region'
)
xNodeList = lapply(1:length(yrs), function(ii){
  slice = unique( data[ which( 
    data$year==yrs[ii] & 
      data$ccode1 %in% cntriesT[[ii]] & 
      data$ccode2 %in% cntriesT[[ii]]
  ), c('ccode1', nVars) ] )
  if(nrow(slice)!=length(cntriesT[[ii]])){ stop('# rows dont match')  }
  regionSplit = model.matrix(~region-1, data=slice)
  adj = data.matrix(cbind( slice[,nVars[-length(nVars)]], regionSplit ))
  rownames(adj) = slice$ccode1
  return( adj[ cntriesT[[ii]], ]  )
})

# save dfs
save(yList, xDyadList, xNodeList, file=paste0(dataPath, 'amenData.rda'))





