loadPkg(c('ROCR', 'caTools','RColorBrewer'))

# Roc curve, depends ROCR
roc = function(prediction, actual){
      pred = prediction(prediction, actual)
      perf = performance(pred,"tpr","fpr")
      rocData = data.frame(attributes(perf)$x.values[[1]], attributes(perf)$y.values[[1]])
      names(rocData) = c('FPR', 'TPR')
      return(rocData)
}

# Auc, depends ROCR
getAUC = function(prediction, actual){
	pred = prediction(prediction, actual)	
	attributes(performance(pred,"auc"))$y.values[[1]]
}

# Plot roc curves, depends RColorBrewer
# plot_type is "roc" or "pr"
rocPlot = function(
  rocData, type='roc', legPos=c(.56,.25), colorPal = 'Set1', 
  colorManual=NULL, linetypes, legText=6, legSpace=3){

	if(type=='roc'){ 
    tmp=ggplot(rocData, aes(x=FPR, y=TPR, color=model, linetype=model)) + 
      geom_abline(intercept=0, slope=1, color='darkgrey') + 
      ylab('True Positive Rate (Sensitivity)') + xlab('False Positive Rate (1-Specificity)')

  }

  if(type=='pr'){ 
    tmp=ggplot(rocData, aes(x=rec, y=prec, color=model, linetype=model)) + 
      ylab('Precision') + xlab('Recall (True Positive Rate)')
  }

  if(is.null(colorManual)){
    tmp = tmp + scale_color_brewer(palette=colorPal)
  } else {
    tmp = tmp + scale_color_manual(values=colorManual)
  }

	tmp=tmp + 
    geom_line(lwd=1) +
    ylim(0,1) + 
    scale_linetype_manual(values=linetypes) + 
    # theme_light(base_family="Source Sans Pro") + 
    theme(
  		legend.position=legPos, legend.title=element_blank(),
      legend.background=element_blank(), 
      legend.text.align = 0, legend.text=element_text(size=legText),
      legend.key=element_rect(colour = NA, fill = NA), legend.key.size=unit(legSpace,'lines'),
      # axis.text.x=element_text(family="Source Sans Pro Light"),
      # axis.text.y=element_text(family="Source Sans Pro Light"),    
      axis.ticks=element_blank(),    
  		panel.border=element_blank()
		)
	return(tmp)
}

# gg separation plot
# thanks to http://www.peterhaschke.com/r/2013/04/22/SeparationPlot.html
ggSep = function(actual, proba, color, lty, fPath, save=TRUE){
  color = c('white',color)
  sepData = data.frame(actual, proba)
  sepData = sepData[order(sepData$proba),]
  tmp=ggplot(sepData) + 
    geom_rect(
      aes(xmin = 0, xmax = seq(length.out = length(actual)), 
        ymin = 0, ymax = 1), fill = "transparent") +
    geom_linerange(
      aes(color = factor(actual), 
        ymin = 0, ymax = 1, x = seq(length.out = length(actual))), alpha = 0.5) +
    geom_line(aes(y = proba, x = seq(length.out = length(actual)), linetype=lty), lwd = 4) + 
    scale_linetype_manual(values=lty) +
    scale_color_manual(values=color) + 
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0), breaks=seq(0,1,.25)) + 
    theme(
      legend.position='none', 
      panel.grid=element_blank(), panel.border=element_rect(colour = "grey13"),
      axis.ticks=element_blank(),
      axis.text=element_blank(),
      axis.title=element_blank()
      )
  if(save){ ggsave(tmp, file=fPath, width=12, height=2) } else { return(tmp) }
}

ggSepBig = function(actual, proba, color, lty, fPath, cutBy=0.001, save=TRUE){
  sepData = data.frame(actual, proba)
  sepData = sepData[order(sepData$proba),]

  #
  sepData$cutVar = cut(sepData$proba, breaks=quantile(sepData$proba, seq(0, 1, cutBy)) )
  sepDataSumm = sepData %>% group_by(cutVar) %>%
    dplyr::summarize(
      actual=sum(actual), 
      proba=mean(proba) )

  #
  cats = length(unique(sepDataSumm$actual))-1
  color = c('white', rep(color, cats))
  sepDataSumm = sepDataSumm[order(sepDataSumm$proba),]

  tmp=ggplot(sepDataSumm) + 
    geom_rect(
      aes(xmin = 0, xmax = seq(length.out = length(actual)), 
        ymin = 0, ymax = 1), fill = "transparent") +
    geom_linerange(
      aes(
        size=factor(actual), color=factor(actual), ymin=0, ymax=1, x=seq(length.out=length(actual))
        ), alpha = 0.5) +
    geom_line(aes(y = proba, x = seq(length.out = length(actual)), linetype=lty), lwd = 4) + 
    scale_linetype_manual(values=lty) +
    scale_color_manual(values=color) + 
    scale_size_manual( values=c(.5,rep(2, cats)) ) +
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0), breaks=seq(0,1,.25)) + 
    theme(
      legend.position='none', 
      panel.grid=element_blank(), panel.border=element_rect(colour = "grey13"),
      axis.ticks=element_blank(),
      axis.text=element_blank(),
      axis.title=element_blank()
      )
  if(save){ ggsave(tmp, file=fPath, width=12, height=2) } else { return(tmp) }
}

####################################################################
# From: https://github.com/andybega/auc-pr/blob/master/auc-pr.r
# Author: Andy Beger
#   Functions for Precision-recall plot and AUC-PR

#' Area under the ROC curve
auc_roc <- function(obs, pred) {
  pred <- prediction(pred, obs)
  auc  <- performance(pred, "auc")@y.values[[1]]
  return(auc) }

#' Area under Precision-recall curve
auc_pr <- function(obs, pred) {
  xx.df <- prediction(pred, obs)
  perf  <- performance(xx.df, "prec", "rec")
  xy    <- data.frame(recall=perf@x.values[[1]], precision=perf@y.values[[1]])
  xy <- subset(xy, !is.nan(xy$precision))
  res   <- trapz(xy$recall, xy$precision)
  return(res) }

# Function to create raw data needed to plot Precision against recall
# For a vector of observed and predicted, creates x-y coordinates for a ROC or PR curve.
rocdf <- function(pred, obs, data=NULL, type=NULL) {
  if (!is.null(data)) {
    pred <- eval(substitute(pred), envir=data)
    obs  <- eval(substitute(obs), envir=data) }
  
  rocr_xy <- switch(type, roc=c("tpr", "fpr"), pr=c("prec", "rec"))
  rocr_df <- prediction(pred, obs)
  rocr_pr <- performance(rocr_df, rocr_xy[1], rocr_xy[2])
  xy <- data.frame(rocr_pr@x.values[[1]], rocr_pr@y.values[[1]])
  colnames(xy) <- switch(type, roc=c("tpr", "fpr"), pr=c("rec", "prec"))
  return(xy) }
####################################################################

####################################################################
#
ggPerfCurves = function(predDfs, suffix, cutBy=0.001){
  # tabular data
  loadPkg('PRROC')
  aucSumm=do.call('rbind', lapply(predDfs,function(x){
    aucROC=roc.curve(x$pred[x$actual==1], x$pred[x$actual==0])$auc
    aucPR=pr.curve(x$pred[x$actual==1], x$pred[x$actual==0])$auc.integral
    return( c('AUC'=aucROC,'AUC (PR)'=aucPR) ) }) )
  aucSumm = aucSumm[order(aucSumm[,1],decreasing=TRUE),]
  aucSumm = trim(format(round(aucSumm, 2), nsmall=2))

  # roc data
  rocData=do.call('rbind', 
    lapply(predDfs, function(x){
      y = roc.curve(x$pred[x$actual==1], x$pred[x$actual==0], curve=T)$curve[,1:2]
      y = data.frame(y, stringsAsFactors = FALSE) ; names(y) = c('FPR','TPR')
      y$model=unique(x$model);return(y) }))

  # precision recall curve data
  prData=do.call('rbind', 
    lapply(predDfs, function(x){
      y = pr.curve(x$pred[x$actual==1], x$pred[x$actual==0], curve=T)$curve[,1:2]
      y = data.frame(y, stringsAsFactors = FALSE) ; names(y) = c('rec','prec')
      y$model=unique(x$model);return(y) }))

  # model col/lty
  ggCols = c('#d6604d', '#4393c3')
  ggLty = c('dotted', 'solid')

  # Separation plots
  loadPkg(c('png','grid','separationplot'))
  sepPngList = lapply(1:length(predDfs), function(ii){
    fSepPath = paste0(plotPath,suffix,'_sep_',names(predDfs)[ii],'_outSample.png')
    # save as pngs for potential use outside of roc
    tmp = data.frame(act=predDfs[[ii]]$actual, proba=predDfs[[ii]]$'pred')
    ggSepBig(actual=tmp$act, proba=tmp$proba, cutBy=cutBy,
      color=ggCols[ii], lty=ggLty[ii], fPath=fSepPath, save=TRUE )
    sepG = rasterGrob(readPNG(fSepPath), interpolate=TRUE)
    return(sepG)
  })

  # get rid of null model
  rocData$model = factor(rocData$model, levels=levels(rocData$model))
  prData$model = factor(prData$model, levels=levels(prData$model))

  tmp = rocPlot(rocData, linetypes=ggLty, colorManual=ggCols) + 
    guides(linetype = FALSE, color = FALSE)
  yLo = -.04 ; yHi = .14
  for(ii in 1:length(sepPngList)){
    tmp = tmp + annotation_custom(sepPngList[[ii]], xmin=.5, xmax=1.05, ymin=yLo, ymax=yHi)
    yLo = yLo + .1 ; yHi = yHi + .1 }
  tmp = tmp + annotate('text', hjust=0, x=.42, y=seq(0.05,0.15,.1), label=names(predDfs))
  ggsave(tmp, file=paste0(plotPath, suffix, '_roc_outSample.pdf'), width=5, height=5)

  tmp=rocPlot(
    prData, type='pr', legText=12, legPos=c(.25,.35),
    legSpace=2, linetypes=ggLty, colorManual=ggCols) +
    guides(linetype=FALSE, color=FALSE) + 
    annotate('text', hjust=0, x=c(.6, .77, .91), y=.85, 
      label=c('  ', ' AUC\n(ROC)', 'AUC\n(PR)'), size=4) + 
    annotate('text', hjust=0, x=.6, y=c(0.65,.75), 
      label=rev(rownames(aucSumm))) + 
    annotate('text', hjust=0, x=.78, y=c(0.65,.75), 
      label=rev(apply(aucSumm, 1, function(x){paste(x, collapse='    ')})), size=4 )
  ggsave(tmp, file=paste0(plotPath, suffix, '_pr_outSample.pdf'), width=5, height=5)  
}  
####################################################################