# depends on:
# loadPkg(c('reshape2','tidyr','ROCR', 'RColorBrewer', 'caTools'))
# source(paste0(fPth, 'binPerfHelpers.R'))

##################################################################
# function to run k-fold cross validation analysis using glm
glmOutSamp = function(
  yList, xDyadL=NULL, seed=6886, folds=30
){

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
  # divide dataset up into folds where each subsample
  # contains observations from each fold except
  # a particular fold that serves as the
  # holdout sample for perf testing
  dvDf = reshape2::melt(yList)
  dvDf = dvDf[dvDf$Var1!=dvDf$Var2,]
  fDf = reshape2::melt(yListFolds)
  fDf = fDf[fDf$Var1!=fDf$Var2,]
  dvDf$fold = fDf$value
  dvDf$id = with(dvDf, paste(Var1, Var2, L1, sep='_'))

  # cleanup
  rm(yList, yListFolds)
  ##############################

  ##############################
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

  # add id var
  xd$id = with(xd, paste(Var1, Var2, L1, sep='_'))
  ##############################

  ##############################
  # merge covars with dv data
  xd = xd[match(dvDf$id, xd$id),]
  df = cbind(dvDf, xd)
  rm(xd, dvDf)
  ##############################

  ##############################
  # gen model formula
  modForm = formula(
    paste0("value ~ ", paste(ivs, collapse='+')))
  ##############################

  ##############################
  # run model by holding out one
  # fold at a time, predicting
  # out and saving perf results
  outPerf = lapply(1:folds, function(fold){

  	# divide data into train/test
  	train = df[df$fold!=fold,]
  	test = df[df$fold==fold,]

  	# run model
  	mod = glm(
  		modForm,
  		data=train,
  		family=binomial)

  	# set up covars for fold holdout pred
  	testCovars = data.matrix(test[,ivs])
  	testCovars = cbind(int=1, testCovars)

  	# get preds and probs
  	pred = testCovars %*% coef(mod)
  	prob = 1/(1+exp(-pred))

  	# get actual data and combine into
  	# new df for perf eval with NAs removed
  	act = test[,'value']
    res = data.frame(
  		actual=act, pred=prob, fold=fold,
  		stringsAsFactors = FALSE)
  	res = na.omit(res)
    return(res)	 })

  # combine list df elements into single df
  outPerf = do.call('rbind', outPerf)
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
  return(glmOutSamp_wFullSpec)
  ##############################
}
##################################################################
