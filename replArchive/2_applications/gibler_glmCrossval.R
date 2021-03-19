#############################
# set a path
require(here)
pth = paste0(here::here(), '/replArchive/')
fPth = paste0(pth, 'helpers/')
gPth = paste0(pth, '2_applications/application_data/gibler/')

#
source(paste0(fPth, 'loadPkg.R'))
packs = c('reshape2', 'tidyr', 'ROCR', 'RColorBrewer', 'caTools')
loadPkg(packs)

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

#
source(paste0(fPth, 'binPerfHelpers.R'))
##############################

##############################
# load data
load(paste0(gPth, 'giblerData.rda'))
##############################

# params
seed = 6886
folds = 30
cores=10
# yList
# xDyadList
# xNodal.S
# xnodal.R

##############################
# create object with similar length
# and dimensions as yList but the values
# within correspond to the fold that obs
# are sorted into for the out of samp analysis
set.seed(seed)
yListFolds = lapply(yList, function(y){ # assign each y-matrix to a fold
  yFold=matrix(sample(1:folds, length(y), replace=TRUE),
               nrow=nrow(y),ncol=ncol(y), dimnames=dimnames(y))
  diag(yFold) = NA
  return(yFold) })
##############################

##############################
# divide dataset up into folds where each fold
# contains observations from each fold except
# a particular fold which will serve as the
# holdout sample for perf testing
yCrossValTrain = lapply(1:folds, function(f){
  yListMiss = lapply(1:length(yList), function(t){
    foldID = yListFolds[[t]] ; y = yList[[t]]
    foldID[foldID==f]=NA ; y=y*foldID
    return(y) })
  names(yListMiss) = names(yList)
  return(yListMiss) }) ; names(yCrossValTrain) = as.character(1:folds)
##############################

##############################
# add nodal covariates to dyadic covariates


# melt list of covariate arrays
# into a data.frame
xd = reshape2::melt(xDyadList)
xd = tidyr::spread(xd, key = Var3, value = value)
xd$id = paste(xd$Var1, xd$Var2, xd$L1, sep='_')
xd =  xd[which(xd$Var1 != xd$Var2),]

# get vector of ivs
ivs = dimnames(xDyadList[[1]])[[3]]

# clean up var labels, remove any underscores
# as that screws up the formula creation
names(xd) = gsub('_','',names(xd))
ivs = gsub('_', '', ivs)
##############################

##############################
# melt y down into a list of dyadic dfs and merge in covariates
yCrossValTrain = lapply(yCrossValTrain, function(y){
  y = reshape2::melt(y) ; y$id = paste(y$Var1, y$Var2, y$L1, sep='_')
  y = y[which(y$Var1!=y$Var2),]
  matchedIDs = match(y$id, xd$id)
  y = cbind(y, xd[matchedIDs,ivs])
  return(y) })
##############################

##############################
# generate model formula and run glm
modForm = formula(
  paste0("value ~ ", paste(ivs, collapse='+')))
fitCrossVal = lapply(yCrossValTrain, function(glmData){
  glmData$value[glmData$value>1] = 1
  fit = glm(modForm, data=glmData, family='binomial')
  return(fit) })
##############################

##############################
# get preds
f = 1
t = 1


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
  if(length(actual)!=length(prob)){stop('mismatch in preds from glm and actual.')}
  res = data.frame(actual=actual, pred=prob, fold=f, stringsAsFactors = FALSE)
  return(res)
}))
##############################

##############################
# get perf stats
aucByFold=do.call('rbind', lapply(1:folds, function(f){
  slice = na.omit(outPerf[outPerf$fold==f,])
  if(length(unique(slice$actual))==1){ return(NULL) }
  perf=cbind(fold=f,
             aucROC=getAUC(slice$pred, slice$actual),
             aucPR=auc_pr(slice$actual, slice$pred)
  )
  return(perf) } ))
outPerf = na.omit(outPerf)
aucROC=getAUC(outPerf$pred, outPerf$actual)
aucPR=auc_pr(outPerf$actual, outPerf$pred)
##############################

##############################
# organize output
glmOutSamp_wFullSpec = list(
  outPerf=outPerf, aucByFold=aucByFold,
  aucROC=aucROC, aucPR=aucPR )
##############################

lapply(glmOutSamp_wFullSpec, dim)

glmOutSamp_wFullSpec[3:4]
