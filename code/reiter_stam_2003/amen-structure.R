########
# Reiter and Stam 2003
#######

#
rm(list=ls())


# load libraries
packs = c('dplyr', 'ggplot2', 'foreign', 'readr', 'lmtest')

#
source("LoadPkg.R")
source("clusteredSE.R")

#
loadPkg(packs)

# read data and add variables
data = 
  read.dta('/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/input/doublereduce.dta')

# get variables
base_vars = c('sideaa pdemdtar pdemdin personal military single democ contig majpow ally loglsrat advanced dispyrs dspline1 dspline2 dspline3 statea stateb year') %>% 
  strsplit(x = ., split = " ") %>%  unlist()

dyad_vars = c('personal', 'military', 'single', 'democ', 'contig', 
              'ally', 'majpow', 'loglsrat', 'advanced', 
              'dispyrs', 'dspline1', 'dspline2', 'dspline3', 
              'pdemdtar', 'pdemdin')

#


# get sampling frame
dataComp = na.omit(data[, base_vars])
yrs = sort(unique(dataComp$year)) 
cntriesT = lapply(yrs, function(t){ as.character( unique( dataComp$statea[dataComp$year==t] ) ) })


# dv
yVar = 'sideaa'
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
xDyadList = lapply(1:length(yrs), function(ii){
  slice = data[ which( 
    data$year==yrs[ii] & 
      data$statea %in% cntriesT[[ii]] & 
      data$stateb %in% cntriesT[[ii]]
  ), c('statea', 'stateb', dyad_vars) ]
  sliceL = reshape2::melt(slice, id=c('statea','stateb'))
  adj = reshape2::acast(sliceL, statea ~ stateb ~ variable, value.var='value')
  return( adj[ cntriesT[[ii]], cntriesT[[ii]],  ] )
})

# nodal vars
# nVars = c('pdemdtar', 'pdemdin')
# 
# 
# xNode = list()
# 
# for(ii in yrs)
# {
#   # get in monad level
#   temp_dat1 = 
#     filter(data, year == ii) %>% 
#     select(., uppcivcon = uppcivcon1, dem = dem1, trans = trans1, 
#            cname1) %>% 
#     distinct()
#   
#   temp_dat2 = 
#     filter(mod1_dat, year == ii) %>% 
#     select(., uppcivcon = uppcivcon2, dem = dem2, trans = trans2, 
#            cname1 = cname2) %>% 
#     distinct()
#   
#   # join
#   temp_join = rbind(temp_dat1, temp_dat2) %>%  distinct()
#   
#   # put in list
#   xNode[[paste(ii)]] = temp_join
#   print(ii)
# }



# save dfs
save(yList, xDyadList, file=paste0('/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/', 'amenData.rda'))
