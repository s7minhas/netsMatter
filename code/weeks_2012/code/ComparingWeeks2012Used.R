
## Script to explore the "used.dta" bit of data, which
## tells me what observations were used in the Weeks 2012 logit model
## Need this to correctly subset the data to replicate
## the clustered robust standard errors from her paper

rm(list=ls())

## Package loading function


loadPkg=function(toLoad){
  for(lib in toLoad){
  if(! lib %in% installed.packages()[,1])
    { install.packages(lib, repos='http://cran.rstudio.com/') }
  suppressMessages( library(lib, character.only=TRUE) ) }}

## Group packages:
## visualization <- c("ggplot2", 'igraph')
## datawrangling=c('foreign', "scales",'data.table', "lubridate", "tidyr")
## textprocessing <- c('tm', 'XML', 'stm', 'RCurl', 'stringr',
##     'SnowballC', 'topicmodels', 'mclust', 'slam')

## packs=c(visualization, datawrangling, textprocessing)
## loadPkg(packs)

library(foreign)

path <- "~/Dropbox/netsMatter/replications/Weeks2012/replication/data/"

used <- read.dta(paste0(path, "used.dta"))

entire.data <- read.dta(paste0(path, "WeeksAPSR2012.dta"))

##attributes:

 class(used) #data.frame
dim(used) #1,039,284 X90

dim(entire.data) #1,039,284 X90


##what are these three columns?
colnames(used)

class(used$used) #integer
head(used[,85:90])

table(used$used) #766727 is the number of 1s, also N in regression

##subset:

weeks.bestguess <- used[which(used$used==1),]

dim(weeks.bestguess) ##766272 x 90


length(which(is.na(weeks.bestguess))) ##19088719

#####################


regressionVars <- c("mzinit", "machinejlw_1", "juntajlw_1",
                    "bossjlw_1", "strongmanjlw_1", "allotherauts_1",
                    "newregime_1", "democracy_2", "cap_1",
                    "cap_2", "initshare", "dependlow", "majmaj",
                    "minmaj", "majmin", "contigdum", "logdist",
                    "s_wt_glo", "s_lead_1", "s_lead_2", "pcyrsmzinit",
                    "pcyrsmzinits1", "pcyrsmzinits2", "pcyrsmzinits3")

## pull only the regeression variables and the clustering variable
dataSub <- weeks.bestguess[c(regressionVars, "dirdyadid")]

length(which(is.na(dataSub))) #no NAs

## just calling the glm() logit model, without doing the
## "if democracy_1!=.," call

dv <- regressionVars[1]
ivs <- regressionVars[2:length(regressionVars)]

modElements=formula(paste0( dv, '~ ', paste(ivs, collapse=' + ') ))

## translating the synatax of her *1.2 in the replication file:
naieveRep <-glm(modElements, family=binomial(link=logit),
                data=dataSub)


## same as paper
round(summary(naieveRep)$'coefficients',3)

## Clustered standard errors, using clusterSEs package

library(clusterSEs)

## All clustered sdropped. 
clust.im.p <- cluster.im.glm(naieveRep, dat=dataSub,
                             ~ dirdyadid, report = T, drop=TRUE)

#### the clust.im.glm crashes
### so working through by hand:

### based on robust standard errors at:
## goo.gl/Yd1Jfy


summary(naieveRep)

s <- summary(naieveRep)
X <- model.matrix(naieveRep) ##NxK matrix
u2 <- residuals(naieveRep)^2 ## number for each obs

XDX <- 0

for(i in 1:nrow(X)) {
XDX <- XDX + u2[i]*X[i,]%*%t(X[i,])
}

## singularity error at this point. 
## reminder: singular mat is a square mat that is not invertable
step <- t(X)%*%X ## 24x24

XX1 <- solve(step)

## throws error that system is computationally singular
## means: linearly dependent columns

## next step: find covariance matrix, see which
## have a correlation very much above 0

## Reminder: variance-covariance matrix for a set of variables {U}
##is defined as

## sigsq{U}. So off-diagonals are sig{UiUj}.

## vcov for a matrix of uncorrelated variables is a diagonal
## matrix. Want to see sqsqUiUj as close to 0 as possible.

varcov <- vcov(naieveRep)

## easier-to-read version:
#round(varcov, 3)

## even easier to read
## only return covariances over .1:

high.vcov <- ifelse(varcov<0.1, 0, round(varcov,3)) ## dependlow 

round(apply(varcov, 1, function(x) sum(x)), 4)

#########################
## Can I get clustered  robust standard errors for
## model w/o dependlow:
###############################


regVars.up <- c("mzinit", "machinejlw_1", "juntajlw_1",
                    "bossjlw_1", "strongmanjlw_1", "allotherauts_1",
                    "newregime_1", "democracy_2", "cap_1",
                    "cap_2", "initshare", "majmaj",
                    "minmaj", "majmin", "contigdum", "logdist",
                    "s_wt_glo", "s_lead_1", "s_lead_2", "pcyrsmzinit",
                    "pcyrsmzinits1", "pcyrsmzinits2", "pcyrsmzinits3")

## just calling the glm() logit model, without doing the
## "if democracy_1!=.," call

dv.up <- regVars.up[1]
ivs.up <- regVars.up[2:length(regVars.up)]

modElements.up=formula(paste0(dv.up, '~ ', paste(ivs.up, collapse=' + ') ))


rep.nodependlow <-glm(modElements.up, family=binomial(link=logit),
                      data=dataSub)

round(summary(rep.nodependlow)$'coefficients',3)

#### Robust standard errors code, from
## goo.gl/Yd1Jfy

s <- summary(rep.nodependlow)
X <- model.matrix(rep.nodependlow)
u2 <- residuals(rep.nodependlow)^2
XDX <- 0

## Here one needs to calculate X'DX. But due to the fact that
## D is huge (NxN), it is better to do it with a cycle. (from blog
## post comments)

for(i in 1:nrow(X)) {
XDX <- XDX + u2[i]*X[i,]%*%t(X[i,])
}

## now solves

XX1 <- solve(t(X)%*%X)

varcovar <- XX1 %*% XDX %*% XX1

dfc <- sqrt(nrow(X))/sqrt(nrow(X)-ncol(X))

stdh <- dfc*sqrt(diag(varcovar))


t <- rep.nodependlow$coefficients/stdh
p <- 2*pnorm(-abs(t))
results <- cbind(rep.nodependlow$coefficients, stdh, t, p)
dimnames(results) <- dimnames(s$coefficients)

round(results, 3)

## Clustered standard error code from The Tarzan blog:
cl   <- function(dat,fm, cluster){
           attach(dat, warn.conflicts = F)
           library(sandwich)
           M <- length(unique(cluster))
           N <- length(cluster)
           K <- fm$rank
           dfc <- (M/(M-1))*((N-1)/(N-K))
           uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
           vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
           coeftest(fm, vcovCL) }


clust.errr <- cl(dataSub, rep.nodependlow, dataSub$dirdyadid)

## trying the cluster.im.glm on the model without dependlow:

clust.im.p <- cluster.im.glm(rep.nodependlow, dat=dataSub,
                             ~ dirdyadid, report = T, drop=TRUE)

