if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/netsMatter/code/mansfield_milner_2012/setup.R') }

# load data
data = foreign::read.dta(file=paste0(dataPath, 'dyadicdata.dta'))
baseVars = c(
	'nohs_rat2_onset2', 'polity2_04', 'polconiiiA', 'ptadum_new', 'llntradenew', 
	'llnGDP_gled08', 'ldGDP_gled08V2', 'larmconflict', 'latopally', 'fmrcol_new', 
	'lcontig_new', 'lndistance', 'lheg_new', 'pcw89', 'llnGDPratio_g', 'onsetperc2', 'lnewgatt', 
	'nohs2_ptaonspl_r2', 'nohs2_ptaonsp1_r2', 'nohs2_ptaonsp2_r2', 'nohs2_ptaonsp3_r2', 
	'region', 'dyadid','year','ccode1','ccode2'
	)

# get sampling frame
dataComp = na.omit(data[, baseVars])
yrs = sort(unique(dataComp$year)) 
cntriesT = lapply(yrs, function(t){ char( unique( dataComp$ccode1[dataComp$year==t] ) ) })

# dv
yVar = 'nohs_rat2_onset2'
yList = lapply(1:length(yrs), function(ii){
	slice = data[ which( 
			data$year==yrs[ii] & 
			data$ccode1 %in% cntriesT[[ii]] & 
			data$ccode2 %in% cntriesT[[ii]]
			), c('ccode1', 'ccode2', yVar) ]
	adj = reshape2::acast(slice, ccode1 ~ ccode2, value.var=yVar)
	return( adj[ cntriesT[[ii]], cntriesT[[ii]] ] )
})

# dyadic vars
dVars = c(
	'ptadum_new', 'llntradenew', 'larmconflict', 
	'latopally', 'fmrcol_new', 'lcontig_new', 
	'lndistance', 'llnGDPratio_g', 'lnewgatt',
	'nohs2_ptaonspl_r2', 'nohs2_ptaonsp1_r2', 'nohs2_ptaonsp2_r2', 'nohs2_ptaonsp3_r2'
	)
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