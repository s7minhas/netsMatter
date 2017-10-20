##################################################################
#
coefp_colors = c(
	"Positive"=rgb(54, 144, 192, maxColorValue=255),
	"Negative"= rgb(222, 45, 38, maxColorValue=255),
	"Positive at 90"=rgb(158, 202, 225, maxColorValue=255),
	"Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
	"Insignificant" = rgb(150, 150, 150, maxColorValue=255)
	)
##################################################################

##################################################################
# srm variance parameter chart, only use for directed case
#tidyr, dplyr, ggplot2, latex2exp
#coefp_colors
plotVC = function(vcFit, fName, w=7, h=4){
	vc = vcFit %>% data.frame() %>%
		select(.,-ncol(.)) %>%
		gather(key='var',value='value',) %>%
		group_by(var) %>%
		summarize(
			median=median(value),
			lo95=quantile(value,.025),
			lo90=quantile(value,.05),
			hi90=quantile(value,.95), 		
			hi95=quantile(value,.975)
			)

	# clean var name
	vcKey = data.frame(dirty=colnames(vcFit)[-ncol(vcFit)], stringsAsFactors = FALSE) 
	vcKey$clean = c(
		'Within-Sender\nVariance ($\\sigma_{a]^{2}$)',
		'Sender-Receiver\nCovariance ($\\sigma_{ab]$)',
		'Within-Receiver\nVariance ($\\sigma_{b]^{2}$)',
		'Reciprocity ($\\rho$)'
		)

	vc$varClean = vcKey$clean[match(vc$var, vcKey$dirty)]
	vc$varClean = factor(vc$varClean, levels=vcKey$clean[c(1,3,2,4)])
	vc$sig = 'Positive'
	vc$bigLab = 'SRRM Parameters'
	ggVC = ggplot(vc, aes(x=varClean, y=median, color=sig)) + 
		geom_hline(aes(yintercept=0), linetype=2, color = "black") + 
		geom_point(size=2.5) + 
		geom_linerange(aes(ymin=lo95,ymax=hi95), linetype=1, size=.5) + 
		geom_linerange(aes(ymin=lo90,ymax=hi90), linetype=1, size=1.5) + 
		scale_color_manual(values=coefp_colors) + 
		facet_grid(~bigLab) + 
		scale_x_discrete('',labels=TeX(levels(rev(vc$varClean)))) + ylab('') +
		theme(
			legend.position = 'none',
			axis.ticks=element_blank(),
			panel.border=element_blank(),
			axis.text.x=element_text(vjust=-1),
			strip.text.x = element_text(size = 10, color='white',
				angle=0, hjust=.03),
			strip.background = element_rect(fill = "#525252", color='#525252')
			)
	ggsave(ggVC, file=fName, width=7, height=3)
}
##################################################################

##################################################################
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
##################################################################

##################################################################
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
##################################################################

##################################################################
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
##################################################################