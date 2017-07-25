################  rose2004 use glm   ###########
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R') }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R') }
if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R') }
loadPkg('devtools') ; devtools::install_github('s7minhas/amen') ; library(amen)
################

################
# load data
pathResults = "/Users/howardliu/Dropbox/netsMatter/replications/rose2004/"
load(paste0(pathResults, 'amenData_rose.rda'))
#rm(list=c('fit', 'fitDyadCovar', 'fitFullSpec'))

# crossval params
seed=6886
folds=30
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
			foldID[foldID==f]=NA ; y=y*foldID
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


	# run glm
	fitCrossVal = lapply(yCrossValTrain, function(glmData){
		#glmData$value[glmData$value>1] = 1 ## change
		fit = glm(form_mod, data=glmData, family='gaussian')	## --> gaussian
		return(fit)
	})

	# get preds
	outPerf = do.call('rbind', lapply(1:folds, function(f){
		# get probs
		testData = cbind(int=1,yCrossValTrain[[f]][is.na(yCrossValTrain[[f]]$value),names(coef(fitCrossVal[[f]]))[-1]])
		#prob = 1/(1+exp(-as.matrix(testData) %*% coef(fitCrossVal[[f]])))
    prob =  as.matrix(testData) %*% coef(fitCrossVal[[f]])
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

	# get binperfhelpers (not for normal data)
	loadPkg(c('ROCR', 'RColorBrewer', 'caTools'))
	source('/Users/howardliu/netsMatter/code/rose2004/binPerfHelpers.R')

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
	out = list( yCrossValTrain=yCrossValTrain,
		fitCrossVal=fitCrossVal,
		outPerf=outPerf, aucByFold=aucByFold,
		aucROC=aucROC, aucPR=aucPR )
	return(out)
	################
}
################

# run with ame full spec
glmOutSamp_wFullSpec=glmOutSamp(
	glmForm=form_mod )
#glmOutSamp_wFullSpec
# save
save(
	glmOutSamp_wFullSpec,
	file=paste0("/Users/howardliu/desktop/", 'glmCrossValResults.rda')
	) ## take long time
################



# ################
# # run with lag dv
# glmOutSamp_wLagDV=glmOutSamp( glmForm=formula(value ~ lagDV) )
#
# # run with ame full spec
# glmOutSamp_wFullSpec=glmOutSamp(
# 	glmForm=formula(value ~
# 		govActor + postBoko +
# 		riotsAgainst.row + vioCivEvents.row +
# 		riotsAgainst.col + vioCivEvents.col) )
#
# # ame full spec + lag DV
# glmOutSamp_wFullSpecLagDV=glmOutSamp(
# 	glmForm=formula(value ~ lagDV +
# 		govActor + postBoko +
# 		riotsAgainst.row + vioCivEvents.row +
# 		riotsAgainst.col + vioCivEvents.col) )

# save
save(
	glmOutSamp_wFullSpec, glmOutSamp_wLagDV,
	glmOutSamp_wFullSpecLagDV,
	file=paste0(pathResults, 'glmCrossValResults.rda')
	)
################
