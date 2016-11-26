if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/netsMatter/code/mansfield_milner_2012/setup.R') }

# load data
data = foreign::read.dta(file=paste0(dataPath, 'dyadicdata.dta'))

baseVars <- c(
	'nohs_rat2_onset2', 'polity2_04', 'polconiiiA', 'ptadum_new', 'llntradenew', 
	'llnGDP_gled08', 'ldGDP_gled08V2', 'larmconflict', 'latopally', 'fmrcol_new', 
	'lcontig_new', 'lndistance', 'lheg_new', 'pcw89', 'llnGDPratio_g', 'onsetperc2', 'lnewgatt', 
	'nohs2_ptaonspl_r2', 'nohs2_ptaonsp1_r2', 'nohs2_ptaonsp2_r2', 'nohs2_ptaonsp3_r2', 
	'region', 'dyadid','year'
	)
baseData <- na.omit(data[, baseVars])

# GLM binomial with logit link, region fixed efFects
baseModel <- glm(
	nohs_rat2_onset2 ~ polity2_04 + polconiiiA + ptadum_new + llntradenew + 
	llnGDP_gled08 + ldGDP_gled08V2 + larmconflict + latopally + fmrcol_new + 
	lcontig_new + lndistance + lheg_new + pcw89 + llnGDPratio_g + onsetperc2 + 
	lnewgatt + nohs2_ptaonspl_r2 + nohs2_ptaonsp1_r2 + nohs2_ptaonsp2_r2 + 
	nohs2_ptaonsp3_r2 + I(region), 
	data = baseData, 
	family = binomial(link = 'logit'))

# need to cluster standard errors by dyads
loadPkg(c('lmtest','multiwayvcov'))
baseModelVcov = cluster.vcov(model=baseModel, cluster=baseData$dyadid, 
	df_correction = FALSE, leverage = 3)
baseModelSumm = coeftest(baseModel, baseModelVcov)

# save
save(baseModelSumm, file=paste0(resultsPath, 'mm_baseModel.rda'))