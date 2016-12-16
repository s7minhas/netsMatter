function(fit, clustervar){        
  ## clustervar is the variable on which to cluster
  F <- function(x){1/(1+exp(-x))}       # logit CDF
  f <- function(x){exp(x)/(1+exp(x))^2} # logit PDF
  fml <- fit$formula
  ## the data call drops missing observations
  ## because sometimes data fed in keeps NAs.
 
  data <- na.omit(fit$data)
  ##print(dim(data))
  #print(paste0("length of NA values in data is:, "length(which(is.na(data))))
               
  beta <- fit$coef
  vcov <- vcov(fit)
  k <- length(beta)
  print(paste0('k is ', k))      
  y <- fit$y
  print(paste0("length of y is ", length(y)))
        
  m <- dim(table(data$clustervar))    # N of clusters, in Weeks' data: 31874
  xvars <- names(beta)            # Name of covariates ordered accordingly
  xvars <- xvars[2:length(xvars)] # Delete (Intercept)
  xmat <- as.matrix(data[,xvars]) # Design matrix
  xmat <- cbind(1, xmat)          # Add intercept
  xb <- xmat %*% beta             # linear predictor (xb)

  ## Now, obtain clustered s.e.
  u <- ((y==1) * f(xb)/F(xb) + (y==0) * -f(xb)/(1-F(xb)))[,1] * xmat
  u.clust <- matrix(NA, nrow=m, ncol=k)
  fc <- factor(data$clustervar)
  
  ## Loop over covariates in two cases:
  ## first case: if cluster variable is in the regression
  ## second case: if not
  for (i in 1:k){ ## loop over covariates
    u.clust[,i] <- tapply(u[,i], fc, sum) ## sum over dyad
  }
  cl.vcov <- vcov %*% ((m/(m-1)) * t(u.clust)%*%(u.clust)) %*% vcov ## sandwich
  cl.se <- sqrt(diag(cl.vcov)) ## clustered s.e.
  return(list(cl.se, cl.vcov))
}
