# Author: CD
# Purpose (old notes): This code creates a dataframe for OLS & oprobit replication for the last time period, issue area POWs. This data frame is more compatible with the GBME results and doesn't have the same selection issues as original dataframe / approach.

# Setup
setwd("/Users/cassydorff/Dropbox/Research/nothingbutnet/netsMatter/replications/morrow2007/")

library(foreign)
library(network)
######################


## Data
# Read in data which contains dyadic compliance measure
morrow.pows.t5.dat<-read.dta("morrow_pows_t5.dta")

# sender democracy
morrow.pows.t5.dem<-read.dta("morrow_pows_t5_dem.dta")

# dyadic ally data
morrow.pows.t5.ally<-read.dta("morrow_pows_t5_ally.dta")

#not sure why I made this in 2012 but i did:
ally<-c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0)
######################


## OLS
morrow.lm<-lm(violator_4_ordinal_comply~ violator_democracy7 +victim_4_ordinal_comply + ally, data=morrow.pows.t5.dat)

morrow.lm<-lm(violator_4_ordinal_comply~ violator_democracy7 +victim_4_ordinal_comply + ally, data=morrow.pows.t5.dat)
######################


##plots
# Comply varies, dem =1, ally=1 (only thing that actually makes some sense)
data.dem.comp<-data.frame(violator_democracy7=1, victim_4_ordinal_comply=morrow.pows.t5.dat$victim_4_ordinal_comply, ally=1)

p.comp<-data.frame(predict(morrow.lm, data.dem.comp, interval="confidence"))

plot(data.dem.comp$victim_4_ordinal_comply, p.comp$fit, type="l", ylim=c(0,6))
lines(data.dem.comp$victim_4_ordinal_comply, p.comp$lwr, type="l", col="blue")
lines(data.dem.comp$victim_4_ordinal_comply, p.comp$upr, type="l", col="blue") 
######################













