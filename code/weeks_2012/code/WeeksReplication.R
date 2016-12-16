# paths
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	pathData='~/Dropbox/Research/netsMatter/replications/Weeks2012/replication/data/'
}

# load libs
# install/load libraries
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
	}
}
loadPkg(c('foreign', 'lmtest', 'sandwich'))

# load data
weeksData = foreign::read.dta(paste0(pathData, 'WeeksAPSR2012.dta'))

# subset to relev covars
dv = 'mzinit'
ivs = c(
	'machinejlw_1', 'juntajlw_1', 'bossjlw_1', 'strongmanjlw_1',
	'allotherauts_1', 'newregime_1', 'democracy_2', 'cap_1', 'cap_2',
	'initshare', 'dependlow', 'majmaj', 'minmaj', 'majmin', 'contigdum',
	'logdist', 's_wt_glo', 's_lead_1', 's_lead_2', 'pcyrsmzinit',
	'pcyrsmzinits1', 'pcyrsmzinits2', 'pcyrsmzinits3'
	)
other = 'democracy_1'
ids = c('ccode1', 'ccode2', 'year', 'dirdyadid')

# construct data for modelling
modData = na.omit( weeksData[,c(dv,ivs,other,ids)] )

# run glm
modForm = formula(
	paste0(dv, '~', 
		paste(ivs, collapse=' + ')
		)
	)
mod = glm(modForm, data=modData, family=binomial(link='logit'))

# none of the pkgs working so going manual
clust = modData$dirdyadid
clustN = length(unique(clust))
params = length(coef(mod))
u = sandwich::estfun(mod)
uClust = matrix(NA, nrow=clustN, ncol=params)
for(j in 1:params){ uClust[,j]=tapply(u[,j], clust, sum) }
clVcov = vcov(mod) %*% ((clustN/(clustN-1)) * t(uClust) %*% uClust ) %*% vcov(mod)

# feed revised vcov in
# results match with stata
modSumm = lmtest::coeftest(mod, vcov=clVcov)

# save replicated results
save(modSumm, file=paste0(pathData, 'weeks_baseModel.rda'))