rm(list=ls())
resultsPath = '/Users/howardliu/Dropbox/netsMatter/replications/rose2004/outputData/'
pathResults = '/Users/howardliu/Dropbox/netsMatter/replications/rose2004/'
plotPath = '/Users/howardliu/Dropbox/netsMatter/replications/rose2004/graphics/'
#
library(magrittr)
library(ggplot2)
library(stringr)
library(gridExtra)
library(Cairo)
library(reshape2)
source('/Users/howardliu/Dropbox/netsMatter/replications/example/outputResultsEx/helperEx.R')
library(amen)

#load( paste0(resultsPath,'ameFit_k2_v1.rda') ) #; ameFit_k2=ameFit
load('/Users/howardliu/Dropbox/netsMatter/replications/rose2004/amenData_rose.rda')


##### coeff plots #####
# load data
load( paste0(resultsPath,'ameFit_k1_re.rda') ) ; ameFit_k0=ameFit; rm(ameFit)
load( paste0(resultsPath,'ameFit_k1_v1.rda') ) ; ameFit_k1=ameFit; rm(ameFit)
load( paste0(resultsPath,'ameFit_k2_v1.rda') ) ; ameFit_k2=ameFit; rm(ameFit)
load( paste0(resultsPath,'ameFit_k3_v1.rda') ) ; ameFit_k3=ameFit; rm(ameFit)


# sum stats function
summStats = function(x){
  res=c(mu=mean(x),med=median(x),sd=sd(x),quantile(x,probs=c(0.025,0.05,0.95,0.975)))
  round(res, 3)
}

# GLM
load(paste0(pathResults, 'glmModel_rose.rda')) # load GLM mod results
base_mod1 <- baseModelSumm
glmBETA = data.frame(var = rownames(base_mod1),
                     mean = base_mod1[ ,1],
                     sd = base_mod1[ ,2])
rownames(glmBETA) = NULL
glmBETA$lo95 = glmBETA$mean - qnorm(.975)*glmBETA$sd
glmBETA$hi95 = glmBETA$mean + qnorm(.975)*glmBETA$sd
glmBETA$lo90 = glmBETA$mean - qnorm(.95)*glmBETA$sd
glmBETA$hi90 = glmBETA$mean + qnorm(.95)*glmBETA$sd
glmBETA$mod = 'GLM'
glmBETA$med = glmBETA$mean

# drop extras and unnecessary params
glmBETA = glmBETA[-which( glmBETA$var %in% c('(Intercept)')),]
glmBETA = glmBETA[1:17,] ## delete fixed effect, only keep variables
# glmBETA = glmBETA[,-which( names(glmBETA) %in% c('sd'))] # drop sd column

glmBETA$var = c( 'bothin', 'onein', 'gsp', 'ldist', 'lrgdp', 'lrgdppc', 'regional', 'custrict', 'comlang',
                 'border', 'landl', 'island', 'lareap',  'comcol', 'curcol', 'colony', 'comctry')

# AME K0
ameBETA = cbind(ameFit_k0$BETA, rho = ameFit_k0$VC[,'rho'])
ameBETA = t(apply(ameBETA, 2, summStats))
colnames(ameBETA) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA = data.frame(ameBETA, stringsAsFactors = F)
ameBETA$var = rownames(ameBETA) ; rownames(ameBETA) = NULL
ameBETA$mod = 'AME_K0'

# drop extras and unnecessary params
ameBETA = ameBETA[-which(ameBETA$var %in% c('rho')),] # 'rho dropped

ameBETA$var = c( 'bothin', 'onein', 'gsp', 'ldist', 'lrgdp', 'lrgdppc', 'regional', 'custrict', 'comlang',
                 'border', 'landl', 'island', 'lareap',  'comcol', 'curcol', 'colony', 'comctry')

# AME K1
ameBETA1 = cbind(ameFit_k1$BETA, rho = ameFit_k1$VC[,'rho'])
ameBETA1 = t(apply(ameBETA1, 2, summStats))
colnames(ameBETA1) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA1 = data.frame(ameBETA1, stringsAsFactors = F)
ameBETA1$var = rownames(ameBETA1) ; rownames(ameBETA1) = NULL
ameBETA1$mod = 'AME_K1'
# drop extras and unnecessary params
ameBETA1 = ameBETA1[-which(ameBETA1$var %in% c('rho')),] # 'rho dropped

ameBETA1$var = c( 'bothin', 'onein', 'gsp', 'ldist', 'lrgdp', 'lrgdppc', 'regional', 'custrict', 'comlang',
                 'border', 'landl', 'island', 'lareap',  'comcol', 'curcol', 'colony', 'comctry')

# AME K2
ameBETA2 = cbind(ameFit_k2$BETA, rho = ameFit_k2$VC[,'rho'])
ameBETA2 = t(apply(ameBETA2, 2, summStats))
colnames(ameBETA2) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA2 = data.frame(ameBETA2, stringsAsFactors = F)
ameBETA2$var = rownames(ameBETA2) ; rownames(ameBETA2) = NULL
ameBETA2$mod = 'AME_K2'
# drop extras and unnecessary params
ameBETA2 = ameBETA2[-which(ameBETA2$var %in% c('rho')),] # 'rho dropped

ameBETA2$var = c( 'bothin', 'onein', 'gsp', 'ldist', 'lrgdp', 'lrgdppc', 'regional', 'custrict', 'comlang',
                 'border', 'landl', 'island', 'lareap',  'comcol', 'curcol', 'colony', 'comctry')

# AME K3
ameBETA3 = cbind(ameFit_k3$BETA, rho = ameFit_k3$VC[,'rho'])
ameBETA3 = t(apply(ameBETA3, 2, summStats))
colnames(ameBETA3) = c('mean', 'med', 'sd', 'lo95','lo90','hi90','hi95')
ameBETA3 = data.frame(ameBETA3, stringsAsFactors = F)
ameBETA3$var = rownames(ameBETA3) ; rownames(ameBETA3) = NULL
ameBETA3$mod = 'AME_K3'

# drop extras and unnecessary params
ameBETA3 = ameBETA3[-which(ameBETA3$var %in% c('rho')),] # 'rho dropped

ameBETA3$var = c( 'bothin', 'onein', 'gsp', 'ldist', 'lrgdp', 'lrgdppc', 'regional', 'custrict', 'comlang',
                 'border', 'landl', 'island', 'lareap',  'comcol', 'curcol', 'colony', 'comctry')
# combine and clean up
pDat = rbind(glmBETA, ameBETA, ameBETA1, ameBETA2, ameBETA3)


# create groups for plotting
vars = unique(pDat$var)
pDat$group = NA
varOrder = c( 'bothin', 'onein', 'gsp', 'ldist', 'lrgdp', 'lrgdppc', 'regional', 'custrict', 'comlang',
                 'border', 'landl', 'island', 'lareap',  'comcol', 'curcol', 'colony', 'comctry')
pDat$var = factor(pDat$var,levels = varOrder, ordered = F)
pDat$var = factor(pDat$var,levels = rev(levels(pDat$var)))

pDat$group[pDat$mod == "GLM"] = 0
pDat$group[pDat$mod == "AME_K0"] = 1
pDat$group[pDat$mod == "AME_K1"] = 2
pDat$group[pDat$mod == "AME_K2"] = 3
pDat$group[pDat$mod == "AME_K3"] = 4

library(dplyr)
# only k =2 and 3
pDat1 = pDat %>% filter(. , mod == "GLM" | mod == "AME_K2" | mod == "AME_K3")

# rename variables
pDat1$var %>% unique
pDat1$var = str_replace_all(pDat1$var, "bothin", "Both in GATT/WTO") %>%
  str_replace_all(., "onein", "One in GATT/WTO") %>%
  str_replace_all(., "gsp", "GSP") %>%
  str_replace_all(., "ldist", "Log distance") %>%
  str_replace_all(., "lrgdp", "Log product real GDP") %>%
  str_replace_all(., "lrgdppc", "Log product real GDP p/c") %>%
  str_replace_all(., "regional", "Regional FTA") %>%
  str_replace_all(., "custrict", "Currency union") %>%
  str_replace_all(., "comlang", "Common language") %>%
  str_replace_all(., "border", "Land border") %>%
  str_replace_all(., "landl", "Number landlocked") %>%
  str_replace_all(., "island", "Number islands") %>%
  str_replace_all(., "lareap", "Log product land area") %>%
  str_replace_all(., "comcol", "Common colonizer") %>%
  str_replace_all(., "curcol", "Currently colonized") %>%
  str_replace_all(., "colony", "Ever colony") %>%
  str_replace_all(., "comctry", "Common country") 
head(pDat1)
# fixate the var order
pDat1$var = factor(pDat1$var, levels = pDat1$var, ordered = F)
pDat1$var = factor(pDat1$var, levels = rev(levels(pDat1$var)) )

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
                             lwd = 1, position = position_dodge(width = .7))
  zp1 = zp1 + geom_pointrange(aes(x = var, y = mean, ymin = lo95,
                                  ymax = hi95),
                              lwd = 1/2, position = position_dodge(width = .7),
                              shape = 21, fill = "WHITE")
  zp1 = zp1 + coord_flip() + labs(x = "", y = '',
                                  color = 'model type')
  zp1 = zp1 + theme_bw()
  return(zp1)
}



# plot
ggCoef(data = pDat1,group = NULL) ; ggsave(filename = paste0(plotPath, 'rose_coefs_all.pdf'), device = cairo_pdf, width=12, height=11)
# ggCoef(data = pDat, group = 1) #; ggsave(filename = paste0(resultsPath, 'reiter_coefs1.pdf'), device = cairo_pdf, width=7, height=7)
# ggCoef(data = pDat, group = 2) #; ggsave(filename = paste0(resultsPath, 'reiter_coefs2.pdf'), device = cairo_pdf, width=7, height=7)
# ggCoef(data = pDat, group = 3) #; ggsave(filename = paste0(resultsPath, 'reiter_coefs3.pdf'), device = cairo_pdf, width=7, height=7)
# ggCoef(data = pDat, group = 4) #; ggsave(filename = paste0(resultsPath, 'reiter_coefs4.pdf'), device = cairo_pdf, width=7, height=7)









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
            axis.text.x=element_text(angle=0, hjust=1, size=6),
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
desktop ="/Users/howardliu/desktop/"
ABplot_sender = addEffPlot(fit = ameFit, row=TRUE, yList= yList)
ggsave(ABplot_sender,
       file=paste0(plotPath,'ABplot_sender_rose.pdf'),
       width=12, height=11, device=cairo_pdf)

ABplot_receiver = addEffPlot(fit = ameFit, row=FALSE, yList= yList)
ggsave(ABplot_receiver,
       file=paste0(plotPath,'ABplot_receiver_rose.pdf'),
       width=12, height=11, device=cairo_pdf)



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
