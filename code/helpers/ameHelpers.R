# AB Effect plot
getAddEffData = function(fit, row=TRUE, addDegree=FALSE, yList=NULL, orderByDegree=FALSE){
	if(row){addEffData = data.frame(addEff=fit$APM, stringsAsFactors = FALSE) ; yLabel='Sender Effects'}
	if(!row){addEffData = data.frame(addEff=fit$BPM, stringsAsFactors = FALSE) ; yLabel='Receiver Effects'}
	addEffData$actor = rownames(addEffData) ; rownames(addEffData) = NULL
	if(!orderByDegree){
		addEffData$actor = factor(addEffData$actor, 
			levels=addEffData[order(addEffData$addEff),'actor'])
	}
	if(addDegree){
		yArr = listToArray(
			actors=sort(unique(unlist(lapply(yList,rownames)))), 
			Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
		if(row){ degree = sort(apply(yArr, 1, mean, na.rm=TRUE)) }
		if(!row){ degree = sort(apply(yArr, 2, mean, na.rm=TRUE)) }
		if(orderByDegree){ 
			addEffData$actor = factor(addEffData$actor, 
				levels=names(degree) )
		}
		addEffData$var = 'Additive Effect'
		tmp = addEffData ; tmp$addEff = degree[match(tmp$actor,names(degree))] ; tmp$var=' Avg. Degree'
		addEffData = rbind(addEffData, tmp) ; rm(tmp)
	}
	addEffData$max = ifelse(addEffData$addEff>=0,addEffData$addEff,0)
	addEffData$min = ifelse(addEffData$addEff<0,addEffData$addEff,0) 
	return(addEffData)
}

addEffPlot = function(fit, row=TRUE, addDegree=FALSE, yList=NULL, orderByDegree=FALSE, addEffData=NULL){
	if(is.null(addEffData)){
		addEffData = getAddEffData(fit, row, addDegree, yList, orderByDegree)
	}
	if(row){ yLabel='Sender Effects'}
	if(!row){ yLabel='Receiver Effects'}		
	gg = ggplot(addEffData, aes(x=actor, y=addEff)) +
		geom_point() + geom_linerange(aes(ymax=max,ymin=min)) +
		ylab(yLabel) + xlab('') + 
		geom_hline(yintercept=0,color='red') + 
		theme(
			panel.border=element_blank(), axis.ticks=element_blank(),
			# axis.text.x=element_text(angle=45, hjust=1, size=4)
			axis.text.x=element_text(angle=90, hjust=1, size=6)
			)
	if(addDegree){
		gg = gg + facet_wrap(~var, nrow=2, scales='free_y')
	}
	return(gg)
}

# plot function
ggCoef = function(data, group = NULL)
{
  if(!is.null(group))
  {
    zp1 = ggplot(data[data$group == group, ], aes(color = mod))
  } else{
    zp1 = ggplot(data, aes(color = mod))
  }
  zp1 = zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
  zp1 = zp1 + geom_linerange(aes(x = var, ymin = lo90,
                                 ymax = hi90),
                             lwd = 1, position = position_dodge(width = .6))
  zp1 = zp1 + geom_pointrange(aes(x = var, y = mean, ymin = lo95,
                                  ymax = hi95, fatten = 3),
                              lwd = 1/2, position = position_dodge(width = .6),
                              shape = 21, fill = "WHITE")
  zp1 = zp1 + coord_flip() + labs(x = "", y = '', 
                                  color = 'model type')
  zp1 = zp1 + theme_bw() + scale_color_brewer(palette = 'Set1') + 
    theme(
      legend.position='top', legend.title=element_blank(),
      legend.text=element_text(family="Source Sans Pro Light"),
      panel.border=element_blank(),
      axis.ticks=element_blank(),
      axis.text.x=element_text(family="Source Sans Pro Light"),
      axis.text.y=element_text(family="Source Sans Pro Light", hjust=0)
    )
  return(zp1)
}


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