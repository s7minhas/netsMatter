### Replicates results in "Dyadic Analysis in International Relations: A Cautionary Tale" 
### Robert S. Erikson, Pablo M. Pinto, Kelly T. Rader

### Set working directory
setwd('~/Research/netsMatter/code/1_simulation/epr_shuffle_repl/')

library(foreign)
library(plm)
library(dyn)
library(sandwich)
library(lmtest)

data <- read.dta("DyadReplicationPA.dta")

data <- data[order(data$statenuma, data$year), ]

attach(data)


#######  Models in Table 1

# Dyad and Year FE
mod1r <- plm(lrtradeab ~ lrgdpab + lrpopab + allied + min_dem, effect = "twoways", model = "within", index = c("dyadid", "year"), data=data)
tval1r <- (summary(mod1r)$coefficients["min_dem"]) / (sqrt(diag(summary(mod1r)$vcov))["min_dem"])

# Dyad and Year FE plus lag
mod2r <- plm(lrtradeab ~ lrgdpab + lrpopab + allied + min_dem + lagrtrade, effect = "twoways", model = "within", index = c("dyadid", "year"), data=data)
tval2r <- (summary(mod2r)$coefficients["min_dem"]) / (sqrt(diag(summary(mod2r)$vcov))["min_dem"])

####### Randomization tests for models in Table 1

# make a country number list to be randomized

countrya <- unique(statenuma)

countryayear <- unique(cbind(statenuma, year, regimea))
colnames(countryayear) <- c("random.matcha", "year", "regimea")
countryayear<-data.frame(countryayear)

countryb <- unique(statenumb)

countrybyear <- unique(cbind(statenumb, year, regimeb))
colnames(countrybyear) <- c("random.matchb", "year", "regimeb")
countrybyear<-data.frame(countrybyear)

countrylist <- unique(c(countrya,countryb))

# matrices for storing estimates

dim <- 4
nshuf <- 1000

democ.sim1r <- array(NA, c(nshuf,dim), dimnames = list(NULL,c("coef", "se", "test", "p")))
democ.sim2r <- array(NA, c(nshuf,dim), dimnames = list(NULL,c("coef", "se", "test", "p")))


########### start randomization test shuffles

start <- Sys.time() 

for(i in 1:nshuf){
  
  # randomize the countrylist "translator"
  
  randid <- data.frame(countrylist=countrylist, randcountry=sample(countrylist))
  
  # merge translator onto countrya list   
  
  countrya<-data.frame(countrya)
  random.countrya <- merge(countrya, randid, by.x = "countrya", by.y = "countrylist")
  
  # merge onto time series of democracy 
  
  random.dema <- merge(random.countrya, countryayear, by.x = "countrya", by.y = "random.matcha", all=TRUE)
  
  # drop the real country code and rename the random country code in order to merge onto dyad level
  
  random.dem2a <- data.frame(statenuma=random.dema$randcountry, year=random.dema$year, rand.regimea=random.dema$regimea)
  
  # merge translator onto countryb list 	
  
  countryb<-data.frame(countryb)
  random.countryb <- merge(countryb, randid, by.x = "countryb", by.y = "countrylist")
  
  # merge onto time series of democracy 
  
  random.demb <- merge(random.countryb, countrybyear, by.x = "countryb", by.y = "random.matchb", all=TRUE)
  
  # drop the real country code and rename the random country code in order to merge onto dyad level
  
  random.dem2b <- data.frame(statenumb=random.demb$randcountry, year=random.demb$year, rand.regimeb=random.demb$regimeb) 
  
  # merge the scrambled democracy time series back into dyad level
  
  random.alla <- merge(data, random.dem2a, by=c("statenuma","year"), all.x=TRUE)	
  
  random.all <- merge(random.alla, random.dem2b, by=c("statenumb","year"), all.x=TRUE) 	
  
  # Recalculate the minimum regime score in the dyad
  
  random.all$rand.min.dem <- pmin(random.all$rand.regimea, random.all$rand.regimeb)
  
  
  # Dyad and Year FE with the random dem score
  
  out1r <- plm(lrtradeab ~ lrgdpab + lrpopab + allied + rand.min.dem, effect = "twoways", model = "within", index = c("dyadid", "year"), data=random.all)
  
  #store the results
  #coefficients
  democ.sim1r[i,1] <- summary(out1r)$coefficients["rand.min.dem",1] 
  #standard errors
  democ.sim1r[i,2] <- (sqrt(diag(summary(out1r)$vcov))["rand.min.dem"])
  #t stats
  democ.sim1r[i,3] <- democ.sim1r[i,1]/democ.sim1r[i,2]
  
  # Dyad and Year FE plus lag with the random dem score
  
  out2r <- plm(lrtradeab ~ lrgdpab + lrpopab + allied + rand.min.dem + lagrtrade, effect = "twoways", model = "within", index = c("dyadid", "year"), data=random.all)
  
  #store the results
  #coefficients
  democ.sim2r[i,1] <- summary(out2r)$coefficients["rand.min.dem",1] 
  #standard errors
  democ.sim2r[i,2] <- (sqrt(diag(summary(out2r)$vcov))["rand.min.dem"])
  #t stats
  democ.sim2r[i,3] <- democ.sim2r[i,1]/democ.sim2r[i,2]
  
}

#pvalues from random shuffles
democ.sim1r[,"p"]<-2*pt(-abs(democ.sim1r[,"test"]),df=3500)
democ.sim2r[,"p"]<-2*pt(-abs(democ.sim2r[,"test"]),df=3500)


### calculate randomization test p values 
randp1 <- length(which(abs(democ.sim1r[,"test"]) > abs(tval1r))) / nshuf
randp2 <- length(which(abs(democ.sim2r[,"test"]) > abs(tval2r))) / nshuf






################################# FIGURES

###### Figure 1

pdf("figure1.pdf", height = 3, width = 5)
par(mfrow=c(1,2), mar=c(5,2,4,2) + 0.1)


sim <- democ.sim1r
var <- "min_dem"
name <- "Dyad and Year Fixed Effects"
yc <- 50

plot(density(sim[,4], from=0, to=1), type="n", yaxt='n', 
     xlab="p value", ylab="", main=name, mgp = c(2, .5, 0), cex.axis=.7, cex.main=.97, cex.lab=.8) 
title(ylab="density", mgp = c(.6, 1, 0), cex.lab=.8)
lines(density(sim[,4], from=0, to=1))
x <- density(sim[,4], from=0, to=1)$x
y <- density(sim[,4], from=0, to=1)$y
left <- which(x <= .1)
left2 <- which(x <= .05)
polygon(x=c(x[left],rev(x[left])),y=c(y[left],rep(0,length(y[left]))),col="gray90")
polygon(x=c(x[left2],rev(x[left2])),y=c(y[left2],rep(0,length(y[left2]))),col="gray")
error <- which(sim[,4] <=.1)
error.rate <- round(length(error)/1000, digits=2)
xcoord <- quantile(x, probs=.2)
ycoord <- yc
text(x = xcoord, y = ycoord, labels = paste("Chance of Type I error = ", error.rate), adj=0, cex=.7)



sim <- democ.sim2r
var <- "min_dem"
name <- "Dyad and Year Fixed Effects\nwith Dynamics"
yc <- 1.5

plot(density(sim[,4], from=0, to=1), type="n", yaxt='n', 
     xlab="p value", ylab="", main=name, mgp = c(2, .5, 0), cex.axis=.7, cex.main=.97, cex.lab=.8) 
title(ylab="density", mgp = c(.6, 1, 0), cex.lab=.8)
lines(density(sim[,4], from=0, to=1))
x <- density(sim[,4], from=0, to=1)$x
y <- density(sim[,4], from=0, to=1)$y
left <- which(x <= .1)
left2 <- which(x <= .05)
polygon(x=c(x[left],rev(x[left])),y=c(y[left],rep(0,length(y[left]))),col="gray90")
polygon(x=c(x[left2],rev(x[left2])),y=c(y[left2],rep(0,length(y[left2]))),col="gray")
error <- which(sim[,4] <=.1)
error.rate <- round(length(error)/1000, digits=2)
xcoord <- quantile(x, probs=.2)
ycoord <- yc
text(x = xcoord, y = ycoord, labels = paste("Chance of Type I error = ", error.rate), adj=0, cex=.7)

dev.off()


###### Figure 2

pdf("figure2.pdf", height = 3, width = 5)
par(mfrow=c(1,2), mar=c(5,2,4,2) + 0.1)


sim <- democ.sim1r
var <- "min_dem"
name <- "Dyad and Year Fixed Effects"
origcoef <- mod1r$coef[var]
origstat <- tval1r


plot(density(sim[,3]), type="n", xlab="test stat", yaxt='n', main=name, 
     mgp = c(2, .5, 0), cex.axis=.7, cex.main=.97, cex.lab=.8) 
title(ylab="density", mgp = c(.6, 1, 0), cex.lab=.8)
lines(density(sim[,3]))
abline(v=origstat, lty=2)
x <- density(sim[,3])$x
y <- density(sim[,3])$y
left <- which(x <= quantile(sim[,3], probs=.05))
right <- which(x >= quantile(sim[,3], probs=.95))
left2 <- which(x <= quantile(sim[,3], probs=.025))
right2 <- which(x >= quantile(sim[,3], probs=.975))
polygon(x=c(x[left],rev(x[left])),y=c(y[left],rep(0,length(y[left]))),col="gray90")
polygon(x=c(x[right],rev(x[right])),y=c(y[right],rep(0,length(y[right]))),col="gray90")
polygon(x=c(x[left2],rev(x[left2])),y=c(y[left2],rep(0,length(y[left2]))),col="gray")
polygon(x=c(x[right2],rev(x[right2])),y=c(y[right2],rep(0,length(y[right2]))),col="gray")


sim <- democ.sim2r
var <- "min_dem"
name <- "Dyad and Year Fixed Effects\nwith Dynamics"
origcoef <- mod2r$coef[var]
origstat <- tval2r


plot(density(sim[,3]), type="n", xlab="test stat", yaxt='n', main=name, 
     mgp = c(2, .5, 0), cex.axis=.7, cex.main=.97, cex.lab=.8) 
title(ylab="density", mgp = c(.6, 1, 0), cex.lab=.8)
lines(density(sim[,3]))
abline(v=origstat, lty=2)
x <- density(sim[,3])$x
y <- density(sim[,3])$y
left <- which(x <= quantile(sim[,3], probs=.05))
right <- which(x >= quantile(sim[,3], probs=.95))
left2 <- which(x <= quantile(sim[,3], probs=.025))
right2 <- which(x >= quantile(sim[,3], probs=.975))
polygon(x=c(x[left],rev(x[left])),y=c(y[left],rep(0,length(y[left]))),col="gray90")
polygon(x=c(x[right],rev(x[right])),y=c(y[right],rep(0,length(y[right]))),col="gray90")
polygon(x=c(x[left2],rev(x[left2])),y=c(y[left2],rep(0,length(y[left2]))),col="gray")
polygon(x=c(x[right2],rev(x[right2])),y=c(y[right2],rep(0,length(y[right2]))),col="gray")

dev.off()


