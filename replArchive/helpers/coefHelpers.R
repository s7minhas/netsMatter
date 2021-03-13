getAmeCoef = function(fit, addTypeLabel=FALSE){
	tmp<-cbind(
		apply(fit$BETA,2,mean), apply(fit$BETA,2,sd),
		apply(fit$BETA,2,mean)/apply(fit$BETA,2,sd),
		2*(1-pnorm( abs(apply(fit$BETA,2,mean)/apply(fit$BETA,2,sd)))),
		t( apply(fit$BETA, 2, function(x){ quantile(x, c(0.025,0.5,0.95,0.975))  }) )
		)
	colnames(tmp)<-c("pmean","psd","z-stat","p-val", 'lo95','lo90','hi90','hi95') 
	if(!addTypeLabel){
		rownames(tmp) = gsub('.dyad', '', rownames(tmp))
		rownames(tmp) = gsub('.row', '', rownames(tmp))
	}
	rownames(tmp)[rownames(tmp)=='intercept'] = '(Intercept)'
	return(tmp)
}