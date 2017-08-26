################
# workspace
rm(list=ls())
if(Sys.info()['user']=='s7m'){
  dropPath = '~/Dropbox/Research/netsMatter/replications/gibler_2017/'
  resultsPath = paste0(dropPath, 'outputData/') 
  fPth = paste0('~/Research/netsMatter/code/helpers/')
}

source(paste0(fPth, 'loadPkg.R'))
packs = c('reshape2', 'tidyr')
loadPkg(packs)

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

#
loadPkg(c('ROCR', 'RColorBrewer', 'caTools'))
source(paste0(fPth, 'binPerfHelpers.R'))
################

################
# load data
load(paste0(dropPath, 'amenData_gibler2.rda'))
xDyadList = lapply(xDyadList, function(xD){
  dimnames(xD)[[3]][which(
    dimnames(xD)[[3]] %in%
    paste0('_spline',1:3) )] = paste0('spline',1:3)
  return(xD) })

#
ids = c('ccode1', 'ccode2', 'year', 'dyad')
dv = 'value'
ivs = c('allied','jointdem', 'cwpceyrs_bkt',
  'spline1', 'spline2', 'spline3', 'contig', 'parity4',
  'firstparity', 'riv1' )
modForm = paste(ivs, collapse=' + ') %>% paste0(dv, ' ~ ', .) %>% formula()

# crossval params
seed=6886
cores=folds=4
################

################
glmOutSamp = function(glmForm){

  ################
  # divide dataset into folds
  set.seed(seed)
  yListFolds = lapply(yList, function(y){
    yFold=matrix(sample(1:folds, length(y), replace=TRUE),
      nrow=nrow(y),ncol=ncol(y), dimnames=dimnames(y))
    diag(yFold) = NA
    return(yFold) })
  ################
  
  ################
  # run models by fold
  yCrossValTrain = lapply(1:folds, function(f){
    yListMiss = lapply(1:length(yList), function(t){
      foldID = yListFolds[[t]] ; y = yList[[t]]
      foldID[foldID==f]=NA ; foldID[foldID != f] = 1
      y=y*foldID
      return(y) })
    names(yListMiss) = names(yList)
    return(yListMiss) }) ; names(yCrossValTrain) = as.character(1:folds)
  
  # melt into glm format
  yCrossValTrain = lapply(yCrossValTrain, function(y){
    y = melt(y) ; y$id = paste(y$Var1, y$Var2, y$L1, sep='_')
    xd = melt(xDyadList)
    xd = tidyr::spread(xd, key = Var3, value = value)
    xd$id = paste(xd$Var1, xd$Var2, xd$L1, sep='_')
    glmData = y
    for(v in ivs){
      glmData$tmp = xd[match(glmData$id,xd$id),v]
      names(glmData)[ncol(glmData)] = v }
    glmData = glmData[which(glmData$Var1!=glmData$Var2),]
    return(glmData)	
  })
  
  # run glm
  fitCrossVal = lapply(yCrossValTrain, function(glmData){
    glmData$value[glmData$value>1] = 1
    fit = glm(glmForm, data=glmData, family='binomial')	
    return(fit)
  })
  
  # get preds
  outPerf = do.call('rbind', lapply(1:folds, function(f){
    # get probs
    testData = cbind(
      int=1,
      yCrossValTrain[[f]][
          is.na(yCrossValTrain[[f]]$value),
          names(coef(fitCrossVal[[f]]))[-1]
        ]
      )
    prob = 1/(1+exp(-as.matrix(testData) %*% coef(fitCrossVal[[f]])))
    
    # get actual
    actual=unlist(lapply(1:length(yListFolds), function(t){
      foldID = yListFolds[[t]] ; y = yList[[t]]
      foldID[foldID!=f]=NA ; foldID[!is.na(foldID)] = 1
      # y=c(y*foldID) ; return(y[!is.na(y)])      
      return( y[!is.na(foldID)] )
    }))
    if(length(actual)!=length(prob)){stop('shit went wrong.')}
    res = data.frame(actual=actual, pred=prob, fold=f, stringsAsFactors = FALSE)
    if(any(grepl('lagDV',glmForm))){res=na.omit(res)}
    return(res)
  }))
  
  # get perf stats
  aucByFold=do.call('rbind', lapply(1:folds, function(f){
    slice = na.omit(outPerf[outPerf$fold==f,])
    if(length(unique(slice$actual))==1){ return(NULL) }
    perf=cbind(fold=f,
               aucROC=getAUC(slice$pred, slice$actual),
               aucPR=auc_pr(slice$actual, slice$pred)
    )
    return(perf) } ))
  aucROC=getAUC(outPerf$pred, outPerf$actual)
  aucPR=auc_pr(outPerf$actual, outPerf$pred)
  ################
  
  ################
  out = list(
    outPerf=outPerf, aucByFold=aucByFold,
    aucROC=aucROC, aucPR=aucPR )
  return(out)
  ################
}
################

################

# run with ame full spec
glmOutSamp_wFullSpec=glmOutSamp( glmForm=modForm )

# save
save(
  glmOutSamp_wFullSpec,
  file=paste0(resultsPath, 'glmCrossVal.rda')
)
################