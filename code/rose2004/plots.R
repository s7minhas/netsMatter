rm(list=ls())
resultsPath = '/Users/howardliu/Dropbox/netsMatter/replications/rose2004/outputData/'
plotPath = '/Users/howardliu/Dropbox/netsMatter/replications/rose2004/outputData/'
#
library(magrittr)
library(ggplot2)
library(stringr)
library(gridExtra)
library(Cairo)
library(reshape2)
source('/Users/howardliu/Dropbox/netsMatter/replications/example/outputResultsEx/helperEx.R')
library(amen)

load( paste0(resultsPath,'ameFit_k2_v1.rda') ) #; ameFit_k2=ameFit
load('/Users/howardliu/Dropbox/netsMatter/replications/rose2004/amenData_rose.rda')

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

addEffData = getAddEffData(fit = ameFit)
rm(addEffData)
addEffPlot = function(fit, row=TRUE, addDegree=FALSE, yList=NULL, orderByDegree=FALSE, addEffData=NULL){
    if(is.null(addEffData)){
        addEffData = getAddEffData(fit, row, addDegree, yList, orderByDegree)
        # convert to ccode by ifs (Rose2004 used)
        ccode = read.csv("/Users/howardliu/Dropbox/netsMatter/replications/rose2004/ifs_countrycode.csv", header=T) 
        #ccode = ccode %>% .[complete.cases(.),]
        # delete NA
        ccode$code = as.factor(ccode$code)
        addEffData = left_join(addEffData, ccode, by =c("actor" = "code") )
        addEffData = dplyr::select(addEffData, addEff, actor =countryname , max, min)
        addEffData = addEffData[order(addEffData$addEff),]
        addEffData$actor = factor(addEffData$actor, levels = addEffData$actor)
    }
    if(row){ yLabel='Sender Effects'}
    if(!row){ yLabel='Receiver Effects'}
    gg = ggplot(addEffData, aes(x=actor, y=addEff )) +
        geom_point() + geom_linerange(aes(ymax=max,ymin=min)) +
        coord_flip() +
        theme(
            panel.border=element_blank(), axis.ticks=element_blank(),
            #text = element_text(size=2),
            axis.text.x=element_text(angle=180, hjust=1, size=1),
            axis.text.y = element_text(size=4,angle=0,hjust=1,vjust=0,face="plain")
            # axis.text.x=element_text(angle=90, hjust=1, size=6)
            ) +
        geom_hline(yintercept=0,color='red') +
        ylab(yLabel) + xlab('') 
        
        
    if(addDegree){
        gg = gg + facet_wrap(~var, nrow=2, scales='free_y')
    }
    return(gg)
}
addEffPlot(fit = ameFit, row=TRUE, yList= yList)
addEffPlot(fit = ameFit, row=FALSE, yList= yList)




######################################################
# [3] nodal effects and multiplicative effects plots
######################################################

#create a list of actor names from your data, this should be in your original dataframe
vNameKey<-list()
# or vNameKey = rownames(ameModel$Y)

# load data
#load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
#yrs = char(2000:2016) ; yList = yList[yrs]
yList %>% length
yList2 = yList[1:3]

#load(paste0(pathResults, 'ameResults.rda')) # load AME mod results


# subset data
#yList2 = yList[42:47]
yArr = listToArray(actors=sort(unique(unlist(lapply(yList,rownames)))),
                   Y=yList2, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
yArrSumm = apply(yArr, c(1,2), sum, na.rm=TRUE)
diag(yArrSumm) = 0

nrow(yArrSumm)
# fix actor names
# rownames(yArrSumm) =
#   countrycode::countrycode(rownames(yArrSumm), origin = 'cown', 'country.name', warn = T)
# colnames(yArrSumm) =
#   countrycode::countrycode(colnames(yArrSumm), origin = 'cown', 'country.name', warn = T)

################
uvCols = brewer.pal(11, 'RdBu')[c(11-2, 3)]
circPlot=ggCirc(
  Y=yArrSumm, U=ameFit$U, V=ameFit$V, vscale=.6,
   force=3,
  lcol='gray85', lsize=.05) +
  scale_color_manual(values=uvCols)
circPlot
# options()$expression  --> look
# ggsave(circPlot,
#        file=paste0(resultsPath,'reiter_circPlot.pdf'),
#        width=12, height=10, device=cairo_pdf)
################


################
# plot vecs on 2d
uDF = data.frame(ameFit$U) ; uDF$name = rownames(uDF) ; uDF$type='Sender Factor Space'
vDF = data.frame(ameFit$V) ; vDF$name = rownames(vDF) ; vDF$type='Receiver Factor Space'
uvDF = rbind(uDF, vDF) ; uvDF$type = factor(uvDF$type, levels=unique(uvDF$type))
#uvDF$name = countrycode::countrycode(uvDF$name, 'cown', 'country.name', warn = T) # problem
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
ggsave(
  file=paste0(resultsPath,'rose_2dPlot.pdf'),
   width=12, height=10, device=cairo_pdf)
