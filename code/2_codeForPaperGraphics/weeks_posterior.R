# setup ###########################################
rm(list=ls())

if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	resultsPath = '~/Dropbox/Research/netsMatter/replications/Weeks2012/replication/output/'
	plotPath = '~/Research/netsMatter/paper/graphics/' }

source('~/Research/netsMatter/code/helpers/functions.R')
source('~/Research/netsMatter/code/helpers/ameHelpers.R')
source('~/Research/netsMatter/code/helpers/binPerfHelpers.R')
source('~/Research/netsMatter/code/helpers/stargazerHelpers.R')
############################################

# modData ###########################################
load( paste0(resultsPath,'model_k2.rda') )
load(paste0(resultsPath,'weeks_glmfit.rda'))
############################################

# # coefSumm ###########################################
# ameSumm = t(apply(ameFit$BETA, 2, function(x){ c(
# 	'Estimate'=mean(x), 'Std. Error'=sd(x), 'z value'=mean(x)/sd(x),
# 	'Pr(>|z|)'=2*(1-pnorm( abs(mean(x)/sd(x)))) )}))
# rownames(ameSumm) = gsub('.row','',rownames(ameSumm),fixed=TRUE)
# rownames(ameSumm) = gsub('.col','',rownames(ameSumm),fixed=TRUE)
# rownames(ameSumm) = gsub('.dyad','',rownames(ameSumm),fixed=TRUE)
# rownames(ameSumm) = gsub('_s','s',rownames(ameSumm),fixed=TRUE)
# rownames(ameSumm) = gsub('intercept','(Intercept)',rownames(ameSumm),fixed=TRUE)
# glmSumm = modSumm[,]
# probSumm = proSumm[,]

# modList = list(glmSumm, probSumm, ameSumm)
# modNames = c('GLM (Logit)','GLM (Probit)', 'AME')

# varKey = data.frame(
# 	dirty=names(coef(mod)),
# 	clean=c( '(Intercept)', 
# 		"Machine", "Junta", "Boss", "Strongman",
# 		"Other Type","New/Unstable Regime", "Democracy Target",
# 		"Military Capabilities Initiator",
# 		"Military Capabilities Target ",
# 		"Initator Share of Capabilities ",## remember this has no AMEN equivalent
# 		"Low Trade Dependence ",
# 		"Both Major Powers", "Minor/Major",
# 		"Major/Minor", "Contiguous", "Log Dist. Between Capitals",
# 		"Alliance Similarity Dyad ",
# 		"Alliance Similarity With System Leader Initiator",
# 		"Alliance Similarity Leader Target",
# 		"Time Since Last Conflict", "Spline1", "Spline2", "Spline3"),
# 	stringsAsFactors = FALSE )
# varKey = varKey[-11,]

# #
# getCoefTable(varKey, modList, modNames, 'weeks', 'Weeks (2012)', plotPath, 3, 'scriptsize')
# ############################################

# # plot srm var ###########################################
# plotVC(ameFit$VC, paste0(plotPath, 'weeks_srmvc.pdf'), w=7, h=4)
# ############################################

# outPerf ###########################################
load(paste0(resultsPath, 'outsampResults2.rda')) # ameOutSamp_NULL
ameOutSamp=ameOutSamp_NULL ; rm(ameOutSamp_NULL)
ameOutSamp$outPerf$pred = 1/(1+exp(-ameOutSamp$outPerf$pred))
load(paste0(resultsPath,'weeksOutPerf_small.rda')) # glmOutSamp

# org
predDfs = list(
	GLM = data.frame(actual=glmOutSamp$outPerf$actual, pred=glmOutSamp$outPerf$pred, model='GLM'),
	AME = data.frame(actual=ameOutSamp$outPerf$actual, pred=ameOutSamp$outPerf$pred, model='AME') )

# run
ggPerfCurves(predDfs, 'weeks')
############################################

# mult eff analysis ###########################################
# load AME model data
load(paste0(resultsPath, 'WeeksamenData.rda')) ## xDyadList, xNodeList.R, xNodeList.s, Ylist

# # subset data
yArr = listToArray(
	actors=sort(unique(unlist(lapply(yList,rownames)))),
	Y=yList[42:47], Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
yArrSumm = apply(yArr, c(1,2), sum, na.rm=TRUE) ; diag(yArrSumm) = 0

# fix actor names
cntryKey = rownames(yList[[length(yList)]]) %>%
	data.frame(code=., cname=countrycode(.,'cown','country.name'),stringsAsFactors = FALSE)
cntryKey$cname[cntryKey$code==731]='North Korea'
cntryKey$cowc = countrycode(cntryKey$cname, 'country.name', 'cowc')

# mods to yArr and UV
yArrSumm = yArrSumm[cntryKey$code, cntryKey$code]
rownames(yArrSumm)=colnames(yArrSumm)=cntryKey$cowc
uData = ameFit$U[cntryKey$code,] ; rownames(uData) = cntryKey$cowc
vData = ameFit$V[cntryKey$code,] ; rownames(vData) = cntryKey$cowc

# geo colors for nodes
loadPkg('cshapes')
cmap = wmap = cshp(date=as.Date('2016-1-1'))
wmap@data$cowc = countrycode(wmap@data$COWCODE, 'cown', 'cowc')
wmap@data$cowc[wmap@data$COWCODE==731]='PRK'
wmap = wmap[which(as.character(wmap$cowc) %in% cntryKey$cowc),]
coords=coordinates(wmap) ; rownames(coords)=wmap$cowc
coords=coords[cntryKey$cowc,]

# Create colors
rlon = pi*coords[,1]/180 ; rlat = pi*coords[,2]/180
slon =  (rlon-min(rlon))/(max(rlon)-min(rlon))
slat =  (rlat-min(rlat))/(max(rlat)-min(rlat))
ccols = rgb( slon^2,slat^2,(1-sqrt(slon*slat))^2)
names(ccols) = cntryKey$cowc ; cntryKey$ccols = ccols

# Generate legend map
cmap@data$cowc = countrycode(cmap@data$COWCODE, 'cown', 'cowc')
cmap@data$cowc[cmap@data$COWCODE==731]='PRK'
mapCol = ccols[match(cmap$cowc, cntryKey$cowc)]
mapCol[is.na(mapCol)] = 'grey' ; names(mapCol) = cmap@data$cowc
cmapDF=fortify(cmap,region='FEATUREID') ; names(cmapDF)[6]='FEATUREID' ; cmapDF=join(cmapDF, cmap@data)
ggMap = ggplot() +
	geom_polygon(data=cmapDF, aes(x=long, y=lat,group=group,fill=cowc),color='grey30',size=.05) +
	scale_fill_manual(values=mapCol) +
	coord_equal() + xlab('') + ylab('') +
	theme(
		legend.position = 'none',
		panel.border = element_blank(), panel.grid=element_blank(),
		axis.ticks = element_blank(), axis.line=element_blank(),
		axis.text = element_blank() )
# ggsave(ggMap, file=paste0(plotPath, 'weeks_mapLeg.png'))

# load back in so we can add to circ
loadPkg(c('grid', 'png'))
mapForCirc = rasterGrob(readPNG(paste0(plotPath, 'weeks_mapLeg.png')), interpolate=TRUE)

# plot
toLabel=c("IRN","IRQ","SYR","PRK",'LIB','CHN','RUS','USA','GMY','CAN','UKG','ISR')
other=names(sort(rowSums(yArrSumm) + colSums(yArrSumm), decreasing=TRUE))[1:50]
tots = c(toLabel,other)

ggU = getDataForCirc(Y=yArrSumm[tots,tots], U=uData[tots,], V=vData[tots,], vscale=.65,removeIsolates=FALSE)$uG
ggU = unique(ggU)
ggU$ccols = cntryKey$ccols[match(ggU$actor,cntryKey$cowc)]
ggU$lab = ggU$actor
ggU$lab[!ggU$lab %in% toLabel] = ''
ggU$lPch = ggU$tPch ; ggU$lPch[ggU$lab==''] = 0

weeksCirc = ggplot(ggU, aes(x=X1, y=X2, size=tPch, color=actor)) +
	annotation_custom(mapForCirc, xmin=-.75, xmax=.75, ymin=-.75, ymax=.75) +	
	geom_point(alpha=.9) + scale_size(range=c(4,8)) +
	ylab("") + xlab("") +
	geom_label_repel(aes(label=lab, size=lPch)) +
	scale_color_manual(values=ccols) +
	theme(
		legend.position = 'none',
		panel.border = element_blank(), panel.grid=element_blank(),
		axis.ticks = element_blank(), axis.line=element_blank(),
		axis.text = element_blank()
		) + theme(panel.grid = element_line())
ggsave(weeksCirc, file=paste0(plotPath, 'weeks_circPlot.pdf'), width=8, height=8)

# inference on mult eff
csim <- function(x,y){
  c <- x %*% y / (sqrt( x%*%x * y%*%y  ))
  return(c) }

csimMat <- function(x) {
  cos <- matrix(NA, nrow=ncol(x),ncol=ncol(x),dimnames=list(colnames(x),colnames(x)))
  for(i in 1:ncol(x)) {
  	for(j in i:ncol(x)) {
      vec1 <- x[which(x[,i] & x[,j]),i]
      vec2 <- x[which(x[,i] & x[,j]),j]  
      cos[i,j]= csim(vec1,vec2)
      cos[j,i]=cos[i,j]
    } }
  return(cos) }

tst = melt(csimMat(t(ameFit$U)))
tst = tst[which(tst$Var1 != tst$Var2),]
############################################