## This script to generate data in the format that AME needs

## paths
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
 dataPath='~/Dropbox/netsMatter/replications/McDonald_2004/data/'
}



# load data
data <- foreign::read.dta(paste0(dataPath, 'PTTOFTfvs.dta'))

baseVars = c('cw2mid', 'cw2midspl','cw2midsp1',
    'cw2midsp2', 'cw2midsp3', 'ally','cont1', 'lncaprat','ldep2l',
    'grow61l', 'lpolity42l','s_wt_glo', 'lrgdpch61h', 'lndistan',
         'majpow', 'limpduty0200h','ccode1', 'ccode2', 'year', 'dyadid')

# Get sampling frame
dataComp = na.omit(data[, baseVars])

years = sort(unique(dataComp$year)) #1971-2000


countriesT = lapply(years, function(t){
    as.character(unique(dataComp$ccode1[dataComp$year==t]) ) })

#countriesT
# get count of actors by year
sort(table( unlist( countriesT ) ))

######################
## Check for symmetry

mst = lapply(years, function(t){
   unique(data$ccode1[data$year==t])})


mst2 = lapply(years, function(t){
   unique(data$ccode2[data$year==t])})

difs <- lapply(1:length(years), function(y){
    ##
    setdiff(mst2[[y]], mst[[y]]) 
})

difs2 <- lapply(1:length(years), function(y){
    ##
    setdiff(mst[[y]], mst2[[y]]) 
})


difs ## fails test!
difs2

#######

## correct for asymmetric actor list:
pds = sort(unique(data$year))

actorList = lapply(pds, function(t){
    
    slice = data[data$year==t,c('ccode1' , 'ccode2')]
    sort(unique(slice$ccode1, slice$ccode2))
})

actorList

## remove ccodes 212, 369, and 483
## each only available for one year
countriesT = lapply(countriesT, function(x){ x[x!='212'] })
countriesT = lapply(countriesT, function(x){ x[x!='369'] })
countriesT = lapply(countriesT, function(x){ x[x!='482'] })

## dv

yVar = 'cw2mid'
yList = lapply(1:length(years), function(ii){
	slice = data[ which( 
			data$year==years[ii] & 
			data$ccode1 %in% countriesT[[ii]] & 
			data$ccode2 %in% countriesT[[ii]]
			), c('ccode1', 'ccode2', yVar) ]
	adj = reshape2::acast(slice, ccode1 ~ ccode2, value.var=yVar)
	return( adj[ countriesT[[ii]], countriesT[[ii]] ] )
}) ; names(yList) = years

# dyadic vars
dVars = c(
	'ptadum_new', 'llntradenew', 'larmconflict', 
	'latopally', 'fmrcol_new', 'lcontig_new', 
	'lndistance', 'llnGDPratio_g', 'lnewgatt',
	'nohs2_ptaonspl_r2', 'nohs2_ptaonsp1_r2', 'nohs2_ptaonsp2_r2', 'nohs2_ptaonsp3_r2'
    )

xDyadList = lapply(1:length(years), function(ii){
	slice = data[ which( 
			data$year==years[ii] & 
			data$ccode1 %in% countriesT[[ii]] & 
			data$ccode2 %in% countriesT[[ii]]
			), c('ccode1', 'ccode2', dVars) ]
	sliceL = reshape2::melt(slice, id=c('ccode1','ccode2'))
	adj = reshape2::acast(sliceL, ccode1 ~ ccode2 ~ variable, value.var='value')
	return( adj[ countriesT[[ii]], countriesT[[ii]],  ] )
}) ; names(xDyadList) = years

# nodal vars
nVars = c(
	'polity2_04', 'polconiiiA', 'llnGDP_gled08', 'ldGDP_gled08V2', 
	'lheg_new', 'pcw89', 'onsetperc2', 'region'
	)
xNodeList = lapply(1:length(years), function(ii){
	slice = unique( data[ which( 
			data$year==years[ii] & 
			data$ccode1 %in% countriesT[[ii]] & 
			data$ccode2 %in% countriesT[[ii]]
			), c('ccode1', nVars) ] )
	if(nrow(slice)!=length(countriesT[[ii]])){ stop('# rows dont match')  }
	regionSplit = model.matrix(~region-1, data=slice)
	adj = data.matrix(cbind( slice[,nVars[-length(nVars)]], regionSplit ))
	rownames(adj) = slice$ccode1
	return( adj[ countriesT[[ii]], ]  )
}) ; names(xNodeList) = years

# save dfs
save(yList, xDyadList, xNodeList, file=paste0(dataPath, 'amenData.rda'))
