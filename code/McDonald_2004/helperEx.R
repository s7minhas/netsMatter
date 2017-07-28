# helper functions for exampleOut.R
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
	} }

ggSep = function(actual, proba, color, lty = "dashed", fPath, save=TRUE){
  color = c('white',color)
  sepData = data.frame(actual, proba)
  sepData = sepData[order(sepData$proba),]
  tmp=ggplot(sepData) + 
    geom_rect(aes(xmin = 0, xmax = seq(length.out = length(actual)), ymin = 0, ymax = 1), fill = "transparent") +
    geom_linerange(aes(color = factor(actual), ymin = 0, ymax = 1, x = seq(length.out = length(actual))), alpha = 0.5) +
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


##################################################### from binPerfHelpers.R 
loadPkg(c('ROCR', 'caTools','RColorBrewer', "ggplot2"))

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
rocPlot = function(rocData, type='roc', legPos=c(.56,.25), colorPal = 'Set1', colorManual=NULL, linetypes, legText=6, legSpace=3){

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
    theme_light(base_family="Source Sans Pro") + 
    theme(
      legend.position=legPos, legend.title=element_blank(),
      legend.background=element_blank(), 
      legend.text.align = 0, legend.text=element_text(size=legText),
      legend.key=element_rect(colour = NA, fill = NA), legend.key.size=unit(legSpace,'lines'),
      axis.text.x=element_text(family="Source Sans Pro Light"),
      axis.text.y=element_text(family="Source Sans Pro Light"),    
      axis.ticks=element_blank(),    
      panel.border=element_blank()
    )
  return(tmp)
}

# gg separation plot
# thanks to http://www.peterhaschke.com/r/2013/04/22/SeparationPlot.html
ggSep = function(actual, proba, color, lty, actLineSize=2, fPath, save=TRUE){
  color = c('white',color)
  sepData = data.frame(actual, proba)
  sepData = sepData[order(sepData$proba),]
  tmp=ggplot(sepData) + 
    geom_rect(aes(xmin = 0, xmax = seq(length.out = length(actual)), ymin = 0, ymax = 1), fill = "transparent") +
    geom_linerange(aes(size=factor(actual), color = factor(actual), ymin = 0, ymax = 1, x = seq(length.out = length(actual))), alpha = 0.5) +
    geom_line(aes(y = proba, x = seq(length.out = length(actual)), linetype=lty), lwd = 4) + 
    scale_linetype_manual(values=lty) +
    scale_size_manual(values=c(1,actLineSize)) + 
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



################# from 'ggcirc.R'
loadPkg('ggrepel')
ggCirc = function(
	Y, U=NULL, V=NULL, row.names=rownames(Y), col.names=colnames(Y),
	vscale=.8, prange=c(2,5), lcol='gray85', ltype='dotted', lsize=.5,
	force=1, maxIter = 3e3, removeIsolates=TRUE, uLabel='U', vLabel='V',
	showActLinks=TRUE, geomLabel=TRUE, geomText=FALSE, geomPoint=TRUE, ...
	){
	
	#
	vLogic = is.null(V) ; uLogic = is.null(U)

	if (uLogic) {
	    a <- rowMeans(Y, na.rm = TRUE)
	    b <- colMeans(Y, na.rm = TRUE)
	    Y0 <- Y
	    Y0[is.na(Y)] <- (outer(a, b, "+"))[is.na(Y)]
	    Y0 <- Y0 - mean(Y0)
	    if (!all(Y == t(Y), na.rm = TRUE)) {
	        sY <- svd(Y0)
	        u <- sY$u[, 1:2]
	        v <- sY$v[, 1:2]
	        mu <- sqrt(apply(u^2, 1, sum))
	        mv <- sqrt(apply(v^2, 1, sum))
	        u <- diag(1/mu) %*% u
	        v <- diag(1/mv) %*% v * vscale
	    }
	    if (all(Y == t(Y), na.rm = TRUE)) {
	        eY <- eigen(Y0)
	        bv <- which(abs(eY$val) >= sort(abs(eY$val), decreasing = TRUE)[2])[1:2]
	        u <- eY$vec[, bv]
	        mu <- sqrt(apply(u^2, 1, sum))
	        u <- diag(1/mu) %*% u
	        mv <- mu
	        v <- u
	    }
	}
	if (!uLogic) {
	    if (vLogic) {
	        V <- U
	        vscale <- 1
	    }
	    mu <- sqrt(apply(U^2, 1, sum))
	    mv <- sqrt(apply(V^2, 1, sum))
	    u <- diag(1/mu) %*% U
	    v <- diag(1/mv) %*% V * vscale
	}

	rsum <- apply(abs(Y), 1, sum, na.rm = TRUE)
	csum <- apply(abs(Y), 2, sum, na.rm = TRUE)
	links <- which(Y != 0, arr.ind = TRUE)
	
	# org df for gg
	uG = data.frame(u*1.2)
	uG$actor = rownames(Y)
	uG$tPch = 0 ; uG$tPch[rsum>0] = (mu[rsum>0])^3
	if(removeIsolates){ uG = uG[uG$tPch>0,] }
	uG$tPch = uG$tPch
	
	# add v if supplied
	if(!vLogic){
		vG = data.frame(v*1.2)
		vG$actor = rownames(Y)
		vG$tPch = 0 ; vG$tPch[csum>0] = (mv[csum>0])^3
		if(removeIsolates){ vG = vG[vG$tPch>0,] }
		vG$tPch = vG$tPch
		
		uG$eff = uLabel ; vG$eff = vLabel
		uG = rbind(uG, vG)		
		uG$eff = factor(uG$eff, levels=c(uLabel,vLabel))
		ggCirc = ggplot(uG, aes(x=X1, y=X2,color=eff))
	}
	if(vLogic){
		ggCirc = ggplot(uG, aes(x=X1, y=X2))
	}
	
	# add segments
	if(showActLinks){
		for(i in 1:nrow(links)){
			ggCirc = ggCirc + geom_segment(
				x=u[links[i,1],1]*1.2, y=u[links[i,1],2]*1.2,
				xend=v[links[i,2],1]*1.2, yend=v[links[i,2],2]*1.2,
				color=lcol, linetype=ltype, size=lsize ) }
	}
	if(geomPoint){ ggCirc = ggCirc + geom_point() }
	if(geomLabel){ ggCirc = ggCirc + geom_label_repel(aes(label=actor, size=tPch, ...),
		force=force, max.iter=maxIter) }
	if(geomText){ ggCirc = ggCirc + geom_text_repel(aes(label=actor, size=tPch, ...),
		force=force, max.iter=maxIter) }
	ggCirc = ggCirc + scale_size(range=prange) +
		theme(
			legend.position='none',
			axis.ticks=element_blank(),
			axis.title=element_blank(),
			axis.text=element_blank(),
			panel.border=element_blank(),
			panel.grid=element_blank()
			)
	return(ggCirc)
}
################