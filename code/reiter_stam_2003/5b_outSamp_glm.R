################
# workspace
rm(list=ls())
source('LoadPkg.R')
packs = c('reshape2', 'tidyr')
loadPkg(packs)
pathResults = '/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/'
################

################
# load data
load(paste0(pathResults, 'amenData.rda'))

# crossval params
seed=6886
folds=4
################
base_vars = c('sideaa pdemdtar pdemdin personal military single democ contig majpow ally loglsrat advanced dispyrs dspline1 dspline2 dspline3') %>% 
  strsplit(x = ., split = " ") %>%  unlist()
form_mod = formula(paste0('value ~ ', paste(base_vars[-1], collapse = '+')))
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
      foldID[foldID==f]=NA ; y=y*foldID
      return(y) })
    names(yListMiss) = names(yList)
    return(yListMiss) }) ; names(yCrossValTrain) = as.character(1:folds)
  
  # melt into glm format
  yCrossValTrain = lapply(yCrossValTrain, function(y){
    y = melt(y)
    xd = melt(xDyadList)
    xd = tidyr::spread(xd, key = Var3, value = value)
    glmData = dplyr::left_join(y, xd[,c(base_vars[-1], 'Var1', 'Var2', 'L1')])
    glmData = glmData[which(glmData$Var1!=glmData$Var2),]
    return(glmData)	
  })
  
  # run glm
  fitCrossVal = lapply(yCrossValTrain, function(glmData){
    glmData$value[glmData$value>1] = 1
    fit = glm(form_mod, data=glmData, family='binomial')	
    return(fit)
  })
  
  # get preds
  outPerf = do.call('rbind', lapply(1:folds, function(f){
    # get probs
    testData = cbind(int=1,yCrossValTrain[[f]][is.na(yCrossValTrain[[f]]$value),names(coef(fitCrossVal[[f]]))[-1]])
    prob = 1/(1+exp(-as.matrix(testData) %*% coef(fitCrossVal[[f]])))
    
    # get actual
    actual=unlist(lapply(1:length(yListFolds), function(t){
      foldID = yListFolds[[t]] ; y = yList[[t]]
      foldID[foldID!=f]=NA ; foldID[!is.na(foldID)] = 1
      y=c(y*foldID) ; return(y[!is.na(y)])
    }))
    if(length(actual)!=length(prob)){stop('shit went wrong.')}
    res = data.frame(actual=actual, pred=prob, fold=f, stringsAsFactors = FALSE)
    if(any(grepl('lagDV',glmForm))){res=na.omit(res)}
    return(res)
  }))
  
  # get binperfhelpers
  loadPkg(c('ROCR', 'RColorBrewer', 'caTools'))
  source('binPerfHelpers.R')
  
  # get perf stats
  aucByFold=do.call('rbind', lapply(1:folds, function(f){
    slice = outPerf[outPerf$fold==f,]
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
glmOutSamp_wFullSpec=glmOutSamp(
  glmForm=form_mod )

# save
save(
  glmOutSamp_wFullSpec,
  file=paste0(pathResults, 'glmCrossValResults.rda')
)
################