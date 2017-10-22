getCoefTable = function(varKey, modSumm, modNames, suffix, replName, path=resultsPath, digs=3, textsize='normalsize'){
	# org matrix to store results
	noModels = length(modSumm)
	varsTable=varKey[,1]
	tableResults = matrix('', nrow=2*length(varsTable), ncol=noModels+1)
	tableResults[,1]=rep(varsTable,2)
	colnames(tableResults) = c('Variable',modNames)

	# loop through mods and add stuff for stargazers
	for(ii in 2:ncol(tableResults)){
		temp = modSumm[[ii-1]]
		temp = temp[match(tableResults[,'Variable'], rownames(temp)),]
		estims = temp[1:length(varsTable),'Estimate']
		estims = round(as.numeric(as.character(estims)),digs)
		pvals = abs(temp[1:length(varsTable),'Pr(>|z|)'])
		pvals = round(as.numeric(as.character(pvals)),digs)
		estims = ifelse(pvals<=0.10 & !is.na(pvals) & pvals>0.05, 
			paste('$', estims,'^{\\ast}$',sep=''), estims)
		estims = ifelse(pvals<0.10 & !is.na(pvals) & pvals<=0.05, 
			paste('$', estims,'^{\\ast\\ast}$',sep=''), estims)
		estims = ifelse(is.na(estims),'',estims)
		tableResults[1:length(varsTable),ii] = estims
		serrors = temp[(length(varsTable)+1):nrow(tableResults),'Std. Error']
		serrors = round(as.numeric(as.character(serrors)),digs)
		serrors = paste('(',serrors,')',sep='')
		serrors = ifelse(serrors=='(NA)','',serrors)
		tableResults[(length(varsTable)+1):nrow(tableResults),ii] = serrors }

	tableResults[tableResults=='(0)'] = paste0('(0.',paste(rep(0,digs),collapse=''),')')
	tableResults[tableResults=="0"] = paste0('0.',paste(rep(0,digs),collapse=''))
	tableResults[tableResults=="$0^{\\ast}$"] = paste0('$0.',paste(rep(0,digs),collapse=''),'^{\\ast}$')
	tableResults[tableResults=="$0^{\\ast\\ast}$"] = paste0('$0.',paste(rep(0,digs),collapse=''),'^{\\ast\\ast}$')

	# Reorganizing rows and variable labels
	tableFinal = NULL
	for(ii in 1:length(varsTable)){
		temp = cbind('', t(tableResults[ii+length(varsTable),2:ncol(tableResults)]))
		tableFinal = rbind(tableFinal, tableResults[ii,], temp) }

	temp=varKey[match(tableFinal[,'Variable'], varKey[,1]),2]
	temp[which(is.na(temp))]=tableFinal[,'Variable'][which(is.na(temp))]
	tableFinal[,'Variable']=temp

	caption='Standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p<0.05$ and $p<0.10$, respectively.'
	caption=paste0('Parameter comparison for ', replName, '. ', caption)
	label=paste0('tab:',suffix,'_coef') ; tableName=paste0(path,suffix,'_coeftable.tex')

	# Print to tex
	print.xtable(
		xtable::xtable(
			tableFinal, align=paste0('ll',paste(rep('c',noModels),collapse='')), 
			caption=caption, label=label ),
		include.rownames=FALSE, sanitize.text.function=identity,
		hline.after=c(0,0,nrow(varKey)*2,nrow(varKey)*2),
		size=textsize, file=tableName )
}