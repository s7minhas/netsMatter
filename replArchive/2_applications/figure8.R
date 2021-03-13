#############################
# set a path
require(here)
pth = paste0(here::here(), '/replArchive/')
wPth = paste0(pth, '2_applications/application_data/weeks/')

#
source(paste0(pth, 'helpers/functions.R'))
source(paste0(pth, 'helpers/ameHelpers.R'))

#
library(amen)
##############################

##############################
# load models
load(paste0(wPth,'ameFitWeeks.rda'))
load(paste0(wPth, 'WeeksamenData.rda'))
##############################

##############################
# mult eff analysis
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
ggsave(ggMap, file=paste0(pth, 'weeks_mapLeg.png'))

# load back in so we can add to circ
loadPkg(c('grid', 'png'))
mapForCirc = rasterGrob(readPNG(paste0(pth, 'weeks_mapLeg.png')), interpolate=TRUE)

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
ggsave(weeksCirc, file=paste0(pth, 'figure8.pdf'), width=8, height=8)
##############################
