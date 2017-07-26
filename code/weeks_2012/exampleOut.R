###################################################
# Example code for AMEN descriptives/output
###################################################

dataPath <- "~/Dropbox/netsMatter/replications/Weeks2012/replication/output/"

load(paste0(dataPath, "model_k32017-03-14_v2.rda"))


## load in these functions / run scripts

source('helperEx.R')
library(dplyr)

###################################################
## [1] Coefplots
###################################################


ls()

attributes(ameFit)

## Coefficient estimates:

ameCoefsEstK3 <- t(apply(ameFit$BETA, 2, function(x){summary(x)}))

ameCoefsEstK3$


mods = list()

mods[[1]] = ameModel
mods[[2]] = glmModel
coefData = lapply(mods, function(x){
    x = summary(x)$'coefficients' %>% data.frame(.)
    x$mod = rownames(x)[2] ; x$var = rownames(x)
    rownames(x) = NULL ; names(x)[1:2] = c('mean','sd')
    x = getCIVecs(x) %>% getSigVec(.)
    return(x)
}) %>% do.call('rbind',.)

## clean up vars
modKey = data.frame(dirty=unique(coefData$mod))

class(coefData)


modKey$clean = c('Latent Angle\nDistance', 'Ideal Point\nDistance', 'S-Score', 'Ideal Point &\nS-Score',' Base\nModel')
coefData$modClean = modKey$clean[match(coefData$mod,modKey$dirty)]
coefData$modClean = factor(coefData$modClean, levels=modKey$clean)
varKey = data.frame(dirty=unique(coefData$var))

### Use accurate names of the variables from the models here
varKey$clean = c(
    '(Intercept)', 
    'Latent Angle\nDistance$_{ij,t-1}$',
    'Joint Democracy$_{ij,t-1}$',
    'Capability Ratio$_{ij,t-1}$',
    'Geographically\nContiguous$_{ij,t-1}$',
    'Avg Dyad\nGDP Growth$_{ij,t-1}$',
    'Peace Years$_{ij,t-1}$',
    'Peace Years$^{2}_{ij,t-1}$',
    'Peace Years$^{3}_{ij,t-1}$',
    'Ideal Point\nDistance$_{ij,t-1}$',
    'S-Score$_{ij,t-1}$',
    'Ideal Point\nDistance$_{ij,t-1}$'
    )

###uncomment this if you only want some variables in the coeff plot
#varKey = varKey[c(2,10,12,11,3:6,7:9,1),]
coefData$varClean = varKey$clean[match(coefData$var,varKey$dirty)]
coefData$varClean = factor(coefData$varClean, levels=rev(varKey$clean[-2]))
coefData = coefData[which(!coefData$varClean %in% varKey$clean[8:12]),]

# plot
posDodge = .7
ggCoef=ggplot(coefData, aes(x=varClean, y=mean, color=sig, group=modClean)) + 
    geom_hline(aes(yintercept=0), linetype=2, color = "black") + 
    geom_point(aes(shape=modClean), size=4, position=position_dodge(width = posDodge)) + 
    geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1, position=position_dodge(width = posDodge)) + 
    geom_linerange(aes(ymin=lo95,ymax=hi95),alpha = 1, size = .5, position=position_dodge(width = posDodge)) +    
    scale_colour_manual(values = coefp_colors, guide=FALSE) +
    ylab('') + scale_x_discrete('', labels=TeX(rev(unique(varKey$clean[-2])))) +    
    # ylab(TeX('$\\beta_{p} \\times \\frac{\\sigma_{x_{p}}}{\\sigma_{y}}$')) +
    coord_flip() + 
    theme(
        legend.position='top', legend.title=element_blank(),
        legend.text=element_text(family="Source Sans Pro Light"),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        axis.text.x=element_text(family="Source Sans Pro Light"),
        axis.text.y=element_text(family="Source Sans Pro Light", hjust=0)
    )
ggsave(ggCoef, file=paste0(pathGraphics,'betaEst.pdf'), width=7, height=7, device=cairo_pdf)
################################################



################################################
# [2] AUC / PR plots
################################################


# loop through pds left out
for(dropFromEnd in c(1,5)){
    ########################
    # add forecast perf plots
    load(paste0(pathResults, 'ameForecastResults.rda')) # ame_glm_outSampTime
    aucSumm = melt( lapply(ame_glm_outSampTime, function(pdMod){
        lapply(pdMod, function(stats){ stats[2:3] }) }) )

    # org results
    predList = melt(lapply(ame_glm_outSampTime, function(pdMod){
        lapply(pdMod, function(stats){ stats[1] }) }))
    predDF = predList[
        predList$L1==paste0('last ',dropFromEnd,' pd excluded'),
        -which(names(predList) %in% c('Var1','Var2','L3','variable'))]

    # load in actual data
    load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
    yrs = char(2000:2016) ; yList = yList[yrs]
    y_Out = yList[[ length(yList) - (dropFromEnd - 1) ]]
    act = reshape2::melt(y_Out) ; act = act[act$Var1 != act$Var2,]
    predDF$act = rep(act$value, 3)

    # relabel model names
    modKey = data.frame(dirty=unique(predDF$L2))
    modKey$clean = c('GLM (Covars)', 'GLM (Lag DV + Covars)', 'AME (Covars)')
    predDF$model = modKey$clean[match(predDF$L2, modKey$dirty)]

    # ugh cleanup
    predDF$value[predDF$L2=='glmFull'] = (1/(1+exp(-predDF$value[predDF$L2=='glmFull'])))
    predDF$value[predDF$L2=='glmFullLagDV'] = (1/(1+exp(-predDF$value[predDF$L2=='glmFullLagDV'])))

    # tabular data
    aucSumm=aucSumm[
        aucSumm$L1==paste0('last ',dropFromEnd,' pd excluded'),
        -which(colnames(aucSumm) %in% c('L1'))
        ]
    aucSumm = dcast(aucSumm, L2 ~ L3)[,c(3,2)]
    colnames(aucSumm) = c('AUC', 'AUC (PR)')
    rownames(aucSumm) = rev(modKey$clean)
    aucSumm = trim(format(round(data.matrix(aucSumm), 2), nsmall=2))
    aucSumm = aucSumm[order(aucSumm[,2],decreasing=TRUE),]
    modOrder = rev(rownames(aucSumm))

    # roc data
    rocData = do.call('rbind', 
        lapply(modKey$dirty, function(v){
            x = predDF[predDF$L2==v,]
            y=roc(x$value,x$act);y$model=unique(x$model);return(y) }) )
    rocData$model = factor(rocData$model, levels=modOrder)

    # precision recall curve data
    rocPrData = do.call('rbind', 
        lapply(modKey$dirty, function(v){
            x = predDF[predDF$L2==v,]
            y=rocdf(x$value,x$act,type='pr');y$model=unique(x$model);return(y) }) )
    rocPrData$model = factor(rocPrData$model, levels=modOrder)
    
    # plotting

    # model col/lty
    ggCols = brewer.pal(length(levels(rocData$model)), 'Set1')[c(3,1,2)]
    # ggLty = c('dashed', 'dotdash', 'solid')
    ggLty = c('dotdash', 'dashed', 'solid')

    # Separation plots
    loadPkg(c('png','grid'))
    sepPngList = lapply(1:nrow(aucSumm), function(ii){
        mName = modOrder[ii]
        fSepPath = paste0(pathGraphics,'sep_',mName,'_outSampleForecast_',dropFromEnd,'.png')
        # save as pngs for potential use outside of roc
        ggSep(actual=predDF$act[predDF$model==mName], proba=predDF$value[predDF$model==mName], 
            color=ggCols[ii], lty=ggLty[ii], fPath=fSepPath, save=TRUE )
        sepG = rasterGrob(readPNG(fSepPath), interpolate=TRUE)
        return(sepG)
    })


######################################################
# [3] nodal effects and multiplicative effects plots
######################################################

#source(paste0(fPth, 'actorInfo.R'))
#create a list of actor names from your data, this should be in your original dataframe
vNameKey<-list()
# or vNameKey = rownames(ameModel$Y)

# load data
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
yrs = char(2000:2016) ; yList = yList[yrs]

load(paste0(pathResults, 'ameResults.rda')) # load AME mod results
yArr = listToArray(actors=sort(unique(unlist(lapply(yList,rownames)))), 
	Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
yArrSumm = apply(yArr, c(1,2), sum, na.rm=TRUE)
diag(yArrSumm) = 0


################
uvCols = brewer.pal(11, 'RdBu')[c(11-2, 3)]
circPlot=ggCirc(
	Y=yArrSumm, U=ameFits$base$U, V=ameFits$base$V, vscale=.6, 
	family="Source Sans Pro Light", force=3, 
	lcol='gray85', lsize=.05) +
	scale_color_manual(values=uvCols)
ggsave(circPlot, 
	file=paste0(pathGraphics,'circPlot.pdf'), 
	width=12, height=10, device=cairo_pdf)
################

################
toDrop = grep('(Nigeria)', rownames(yArrSumm))
ySimp = yArrSumm[-toDrop,] ; ySimp = ySimp[,-toDrop] 
uSimp = ameFits$base$U[-toDrop,]
vSimp = ameFits$base$V[-toDrop,]

uvCols = brewer.pal(11, 'RdBu')[c(11-2, 3)]
circPlot=ggCirc(
	Y=ySimp, U=uSimp, V=vSimp, vscale=.7, 
	family="Source Sans Pro Light", 
	force=1, 
	removeIsolates=FALSE, showActLinks=FALSE) +
	scale_color_manual(values=uvCols)
ggsave(circPlot, 
	file=paste0(pathGraphics, 'circPlotSimple.pdf'), 
	width=12, height=10, device=cairo_pdf)
################

################
toDrop = grep('(Nigeria)', rownames(yArrSumm))
ySimp = yArrSumm[-toDrop,] ; ySimp = ySimp[,-toDrop] 
uSimp = ameFits$base$U[-toDrop,]
vSimp = ameFits$base$V[-toDrop,]

uvCols = brewer.pal(11, 'RdBu')[c(11-2, 3)]
facet_labeller = function(string){ TeX(string) }

circPlot=ggCirc(
	Y=ySimp, U=uSimp, V=vSimp, vscale=.7, force=5,
	family="Source Sans Pro Light", 
	uLabel='Groups with Common Sending Patterns ($u_{i}$)',
	vLabel='Groups with Common Receiving Patterns ($v_{j}$)',
	removeIsolates=FALSE, showActLinks=FALSE) +
	scale_color_manual(values=uvCols) +
	facet_wrap(~eff, 
		ncol=2, 
		labeller=as_labeller(facet_labeller, default = label_parsed) ) +
	theme(
		strip.text.x = element_text(size = 16, color='white',
			family="Source Sans Pro Semibold", angle=0, hjust=.2),
		strip.background = element_rect(fill = "#525252", color='#525252')
		)

ggsave(circPlot, 
	file=paste0(pathGraphics, 'circPlotSimplev2.pdf'), 
	width=12, height=6, device=cairo_pdf)
################

################
ySimp = yArrSumm
uSimp = ameFits$base$U
vSimp = ameFits$base$V

uvCols = brewer.pal(11, 'RdBu')[c(11-2, 3)]
facet_labeller = function(string){ TeX(string) }

circPlot=ggCirc(
	Y=ySimp, U=uSimp, V=vSimp, vscale=1, force=5,
	family="Source Sans Pro Light", 
	uLabel='Groups with Common Sending Patterns ($u_{i}$)',
	vLabel='Groups with Common Receiving Patterns ($v_{j}$)',
	removeIsolates=FALSE, showActLinks=FALSE) +
	scale_color_manual(values=uvCols) +
	facet_wrap(~eff, 
		ncol=2, 
		labeller=as_labeller(facet_labeller, default = label_parsed) ) +
	theme(
		strip.text.x = element_text(size = 16, color='white',
			family="Source Sans Pro Semibold", angle=0, hjust=.2),
		strip.background = element_rect(fill = "#525252", color='#525252')
		)

ggsave(circPlot, 
	file=paste0(pathGraphics, 'circPlotSimplev3.pdf'), 
	width=12, height=6, device=cairo_pdf)
################

################
# plot vecs on 2d
uDF = data.frame(ameFits$base$U) ; uDF$name = rownames(uDF) ; uDF$type='Sender Factor Space'
vDF = data.frame(ameFits$base$V) ; vDF$name = rownames(vDF) ; vDF$type='Receiver Factor Space'
uvDF = rbind(uDF, vDF) ; uvDF$type = factor(uvDF$type, levels=unique(uvDF$type))
ggplot(uvDF, aes(x=X1, y=X2, color=type, label=name)) + 
	geom_vline(xintercept = 0, linetype='dashed', color='grey50') + 
	geom_hline(yintercept = 0, linetype='dashed', color='grey50') + 
	scale_color_manual(values=rev(uvCols)) + 
	geom_point() + 
	geom_text_repel() + 
	facet_wrap(~type) + 
	xlab('') + ylab('') + 
	labs(color='') + 
	theme(
		legend.position = 'none',
		axis.ticks=element_blank(),
		axis.text=element_blank(),
		panel.border=element_blank()
		)
################
