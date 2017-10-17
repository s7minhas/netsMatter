rm(list=ls())
library("igraph")
library('RColorBrewer')
set.seed(6886)

#### stoch equiv
# adjMat = matrix(0, nrow=20, ncol=20)
adjMat = matrix(0, nrow=16, ncol=16)
high = 1:8
oth = 9:16
# med = 5:11
# low = 12:16
# oth = 17:20

adjMat[high,high] = rbinom(length(adjMat[high,high]), 1, .4) # high-high
# adjMat[med,med] = rbinom(length(adjMat[med,med]), 1, .6) # med-med
# adjMat[low,low] = rbinom(length(adjMat[low,low]), 1, .6) # low-low
adjMat[oth,oth] = rbinom(length(adjMat[oth,oth]), 1, .4) # low-low

# adjMat[high,med] = rbinom(length(adjMat[high,med]), 1, .2) # high - med
# adjMat[med,high] = rbinom(length(adjMat[med,high]), 1, .25) # med - high

# adjMat[low,high] = rbinom(length(adjMat[low,high]), 1, .0) # low - high
# adjMat[high,low] = rbinom(length(adjMat[high,low]), 1, .0) # high - low

# adjMat[med,low] = rbinom(length(adjMat[med,low]), 1, .1) # med - low
# adjMat[low,med] = rbinom(length(adjMat[low,med]), 1, .3) # low - med

adjMat[oth,high] = rbinom(length(adjMat[oth,high]), 1, .05) # med - high
adjMat[high,oth] = rbinom(length(adjMat[high,oth]), 1, .05) # med - high

# adjMat[oth,low] = rbinom(length(adjMat[oth,low]), 1, .15) # med - low
# adjMat[low,oth] = rbinom(length(adjMat[low,oth]), 1, .15) # med - low

# adjMat[oth,med] = rbinom(length(adjMat[oth,med]), 1, 0) # med - med
# adjMat[med,oth] = rbinom(length(adjMat[med,oth]), 1, 0) # med - med


# # oops undirected
# adjMat[lower.tri(adjMat)] = 0
# adjMat[lower.tri(adjMat)] = t(adjMat)[lower.tri(adjMat)]
# g = graph_from_adjacency_matrix(adjMat, mode='undirected', weighted=NULL, diag=FALSE)

g = graph_from_adjacency_matrix(adjMat, mode='directed', weighted=NULL, diag=FALSE)

# layout
hl=layout.norm(layout_nicely(graph_from_adjacency_matrix(adjMat[high,high], mode='directed', weighted=NULL, diag=FALSE)), 0, .3, .8, 1)
ol=layout.norm(layout_nicely(graph_from_adjacency_matrix(adjMat[oth,oth], mode='directed', weighted=NULL, diag=FALSE)), .7, 1, .8, 1)
# ml=layout.norm(layout_nicely(graph_from_adjacency_matrix(adjMat[med,med], mode='directed', weighted=NULL, diag=FALSE)), 0, .3, 0, .4)
# ll=layout.norm(layout_nicely(graph_from_adjacency_matrix(adjMat[low,low], mode='directed', weighted=NULL, diag=FALSE)), .7, 1, 0, .4)
l = rbind(hl,ol
	# ,ml,ll
	)

# color by class
ccols = brewer.pal(2, 'Set1')
V(g)$color = c(
	rep(ccols[1], length(high)), 
	rep(ccols[2], length(oth))
	# , rep(ccols[3], length(med)), rep(ccols[4], length(low))
	)

#


# plot save
# fName = paste0('~/Research/netsMatter/paper/stochEquivNet.pdf') ; pdf(file=fName, width=7.5, height=3)
plot(g, 
	vertex.label=NA, 
	vertex.color=V(g)$color, 
	# layout=l,
	edge.arrow.size=.25,
	vertex.size=degree(g)+3
	# asp=TRUE
	)
# dev.off() ; system( paste0('pdfcrop ', fName, ' ', fName) )
# system( paste0('open ', fName, ' ', fName) )

library(ggraph)

V(g)$nSize = degree(g)

set.seed(6886)
ggNet=ggraph(g, layout='nicely') + 
    geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) + 
    geom_node_point(aes(color=color, shape=color), size=V(g)$nSize) + 
    scale_color_manual(values=ccols) +
    theme_bw() + 
    theme(
    	axis.ticks=element_blank(),
    	axis.text = element_blank(),
    	axis.title = element_blank(),
    	panel.border=element_blank(),
    	legend.position = 'none',
    	panel.grid = element_blank()
    	)
ggNet

ggsave(ggNet, file=paste0('~/Research/netsMatter/paper/stochEquiv.pdf'), width=8, height=4)