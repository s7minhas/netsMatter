if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/amenRepl/code/mansfield_milner_2012/setup.R') }

# load data
data = foreign::read.dta(file=paste0(dataPath, 'dyadicdata.dta'))

baseVars <- c('nohs_rat2_onset2', 'polity2_04', 'polconiiiA', 'ptadum_new', 'llntradenew', 'llnGDP_gled08', 'ldGDP_gled08V2', 'larmconflict', 'latopally', 'fmrcol_new', 'lcontig_new', 'lndistance', 'lheg_new', 'pcw89', 'llnGDPratio_g', 'onsetperc2', 'lnewgatt', 'nohs2_ptaonspl_r2', 'nohs2_ptaonsp1_r2', 'nohs2_ptaonsp2_r2', 'nohs2_ptaonsp3_r2', 'region', 'dyadid')
baseData <- na.omit(data[, baseVars])

# GLM binomial with logit link, region fixed efFects
baseModel <- glm(
	nohs_rat2_onset2 ~ polity2_04 + polconiiiA + ptadum_new + llntradenew + 
	llnGDP_gled08 + ldGDP_gled08V2 + larmconflict + latopally + fmrcol_new + 
	lcontig_new + lndistance + lheg_new + pcw89 + llnGDPratio_g + onsetperc2 + 
	lnewgatt + nohs2_ptaonspl_r2 + nohs2_ptaonsp1_r2 + nohs2_ptaonsp2_r2 + 
	nohs2_ptaonsp3_r2 + I(region), data = baseData, family = binomial(link = 'logit'))

summary(baseModel)


# check countries by year
cntriesYr = lapply(unique(data$year), function(yr){
	return( unique( data$ccode1[data$year==yr] ) ) })


test = data[which(data$year %in% c(min(data$year), max(data$year)) ), ]


data(YX_bin_long) 

YX_bin_long$X[1,3,1,1] = NA

fit<-ame_rep(YX_bin_long$Y,YX_bin_long$X,burn=5,nscan=5,odens=1,model="bin")
