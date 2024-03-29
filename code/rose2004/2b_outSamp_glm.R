################  rose2004 use glm   ###########
################
# load data
rm(list=ls())

if(Sys.info()['user']=='howardliu'){
  source('~/netsMatter/code/rose2004/loadPkg.R')
  pathResults = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/"
}
if(Sys.info()['user']=='s7m'){
  source('~/Research/netsMatter/code/rose2004/loadPkg.R')
  pathDrop = '~/Dropbox/Research/netsMatter/'
  pathResults = paste0(pathDrop, 'replications/rose2004/')
} 

load(paste0(pathResults, 'amenData_rose.rda'))

#rm(list=c('fit', 'fitDyadCovar', 'fitFullSpec'))

# crossval params
seed=6886
# folds=30
folds=4
################
base_vars = c('ltrade','bothin', 'onein', 'gsp', 'ldist', 'lrgdp', 'lrgdppc', 'regional',
              'custrict',
              'comlang', 'border', 'landl', 'island', 'lareap', 'comcol', 'curcol', 'colony', 'comctry' #, 'as.factor(year)'
)
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
      foldID[foldID==f]=NA ;  foldID[foldID != f] = 1
      y=y*foldID
      return(y) })
    names(yListMiss) = names(yList)
    return(yListMiss) }) ; names(yCrossValTrain) = as.character(1:folds)

  # melt into glm format
  yCrossValTrain = lapply(yCrossValTrain, function(y){
    y = melt(y)
    xd = melt(xDyadList)
    xd = tidyr::spread(xd, key = Var3, value = value)
    glmData = cbind(y,xd[,base_vars[-1]]) ## no sure?
    glmData = glmData[which(glmData$Var1!=glmData$Var2),]
    return(glmData)
  })
foldVec = melt(yListFolds)
foldVec = foldVec[which(foldVec$Var1!=foldVec$Var2),]
  # run glm
  fitCrossVal = lapply(yCrossValTrain, function(glmData){
    #glmData$value[glmData$value>1] = 1 ## change
    fit = lm(form_mod, data=glmData)	## --> gaussian
    return(fit)
  })

    # get preds
  outPerf = do.call('rbind', lapply(1:folds, function(f){
    # get probs
    testData = cbind(int=1,yCrossValTrain[[f]][which(foldVec$value == f),names(coef(fitCrossVal[[f]]))[-1]])
    prob =  as.matrix(testData) %*% coef(fitCrossVal[[f]])
    foldIndex = foldVec[which(foldVec$value == f),]
    foldIndex = foldIndex[which(!is.na(prob)),]
    prob = prob[!is.na(prob)]

        # get actual
    actual = c()
    for(i in 1:dim(foldIndex)[1]){
      ll = foldIndex$L1[i]
      actual = c(actual, yList[[ll]][which(rownames(yList[[ll]]) == foldIndex$Var1[i]), which(rownames(yList[[ll]]) == foldIndex$Var2[i])])
    }
    if(length(actual)!=length(prob)){stop('shit went wrong.')}
    res = data.frame(actual=actual, pred=prob, fold=f, stringsAsFactors = FALSE)
    return(res)
  }))

  ################
  rmse = sqrt(mean((outPerf$actual - outPerf$pred)^2, na.rm = T))
  rmdse = sqrt(median((outPerf$actual - outPerf$pred)^2, na.rm = T))
  ssTOT = sum((outPerf$actual - mean(outPerf$actual, na.rm = T))^2, na.rm = T)
  ssRES = sum((outPerf$pred - outPerf$actual)^2, na.rm = T)
  rmseByFold=do.call('rbind', lapply(1:folds, function(f){
     slice = outPerf[outPerf$fold==f,]
     if(length(unique(slice$actual))==1){ return(NULL) }
     perf=cbind(fold=f,
                rmse=sqrt(mean((slice$actual - slice$pred)^2, na.rm = T)),
                rmdse =sqrt(median((slice$actual - slice$pred)^2, na.rm = T)),
                r2 =1 - sum((slice$pred - slice$actual)^2, na.rm = T)/sum((slice$actual - mean(slice$actual, na.rm = T))^2, na.rm = T))
     return(perf) } ))

  r2 = 1 - ssRES/ssTOT
  out = list( yCrossValTrain=yCrossValTrain,
              fitCrossVal=fitCrossVal,
              outPerf=outPerf, rmseByFold=rmseByFold,
              rmseOUT = rmse, rmdseOUT = rmdse )
  return(out)
  ################
}
################

# run with ame full spec
glmOutSamp_wFullSpec=glmOutSamp(
  glmForm=form_mod )# 5 mins

# save
# save(
#   glmOutSamp_wFullSpec, glmOutSamp_wLagDV,
#   glmOutSamp_wFullSpecLagDV,
#   file=paste0(pathResults, 'glmCrossValResults.rda')
# )

# save, 4 folds
glmOutSamp = glmOutSamp_wFullSpec[3:6]
save(
  glmOutSamp, 
  file=paste0(pathResults, 'glmCrossValResults_4f_small.rda')
)

load(paste0(pathResults, 'glmCrossValResults.rda'))
glmOutSamp_wFullSpec$rmseOUT # 3.226501

################
