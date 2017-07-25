

##### Gartzke 2007 AMEN "Improved" Replication #####
# (A) This replication used monadic-level variables,
# instead of dyadic level summaries, such as
# min or max value for dyad.
# (B) This replication standardizes use of variables
# such as polity, which were used in a non-standard 
# way in the original paper.

rm(list=ls())

library(foreign)
library(splines) # +ns(year),4) in the model
library(plyr)

if(Sys.info()['user']=='jordan'){
  jpr128 = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/JPR128.DTA")
  thirdparty = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/3rdpartyallymod.dta")
  affinity = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/AFFINITY.DTA")
  capabil = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/CAPABIL.DTA")
  distance = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/DISTANCE.DTA")
  dqamt = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/dqamt5099_mod2.dta")
  dyadmid = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/dyadmid60.dta")
  dyadsamp = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/DYADSAMP.DTA")
  econlib = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/ECONLIB.DTA")
  esplccod = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/ESPLCCOD.DTA")
  esplyear = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/ESPLYEAR.DTA")
  freedomhouse = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/freedomhouse.dta")
  gleditschtrade = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/gleditschtrade.dta")
  IMFcap = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/IMFcapdata.dta")
  kaopen_mod2 = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/kaopen_mod2.dta")
  kaopen = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/KAOPEN.DTA")
  natowarsaw = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/natowarsaw.dta")
  polity = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/polity98mod.dta")
  pwtmod = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/PWTMOD.DTA")
  igo = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/similarigo.dta")
  vanhanen = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007/vanhanendem.dta")
}





############################################
# Prepare base data
############################################



# recreate identifier variables
jpr128$dyadidyr=((jpr128$statea*1000000)+(jpr128$stateb*1000)+(jpr128$year-1000))
jpr128$dyadid=((jpr128$statea*1000)+jpr128$stateb)
jpr128$cntryera=((jpr128$statea*10000)+jpr128$year)
jpr128$cntryerb=((jpr128$stateb*10000)+jpr128$year)



### skip Gartzke's irregular "normalization" of polity to a 0-10 scale ###



# levels of democracy interaction 
jpr128$jntdem=(jpr128$demauta*jpr128$demautb)



### skip Gartzke's generation of high and low democracy values ###



# dummies for democracy and joint democracy
jpr128$demdumya = 0
jpr128$demdumya[jpr128$demauta>=7] = 1

jpr128$demdumyb = 0
jpr128$demdumyb[jpr128$demautb>=7] = 1

jpr128$demdumy1 = 0
jpr128$demdumy1[jpr128$demdumya==1 | jpr128$demdumyb==1] = 1

jpr128$demdumy2 = 0
jpr128$demdumy2[jpr128$demdumya==1 & jpr128$demdumyb==1] = 1



### skip Gartzke's generation of high and low trade dependence values ###



# levels of trade dependence interaction 
jpr128$jntdep=jpr128$depa*jpr128$depb



### skip Gartzke's generation of high and low national price dependence values ###



# levels of national price dependence interaction 
jpr128$jtdep_np=jpr128$depa_np*jpr128$depb_np



# generate contiguity variable
jpr128$contig = 0 
jpr128$contig[jpr128$conttype<=4 & jpr128$conttype>0] = 1



# generate major power variables
### I keep the dyadic version of this variable, but also added monadic ###
jpr128$majpA = 0
jpr128$majpA[jpr128$statea==2 | jpr128$statea==200 | jpr128$statea==220 | jpr128$statea==365 | 
               jpr128$statea==710] = 1

jpr128$majpB = 0 
jpr128$majpB[jpr128$stateb==2 | jpr128$stateb==200 | jpr128$stateb==220 | jpr128$stateb==365 | 
               jpr128$stateb==710] = 1

jpr128$majpdyds = 0
jpr128$majpdyds[jpr128$statea==2 | jpr128$statea==200 | jpr128$statea==220 | jpr128$statea==365 | 
                  jpr128$statea==710 | jpr128$stateb==2 | jpr128$stateb==200 | jpr128$stateb==220 | jpr128$stateb==365 | 
                  jpr128$stateb==710] = 1



### To ensure the splines are generated the same way as in Stata,
### I will just merge in Gartzke's splines later.





############################################
# Bring in the DV
############################################

dyadmid = dyadmid[, c('STATEA', 
                      'STATEB',
                      'YEAR',
                      'FATLEV',
                      'HIHOST',
                      'RECIP')]

dyadmid$maoznew = 1

names(dyadmid) = c('statea',
                   'stateb',
                   'year',
                   'fatlev',
                   'hihost',
                   'recip',
                   'maoznew')

dyadmid$dyadidyr=((dyadmid$statea*1000000)+(dyadmid$stateb*1000)+(dyadmid$year-1000))

dyadmid$fatlev[dyadmid$fatlev==-9] = NA
dyadmid$recip[dyadmid$recip==-1] = 1

# remove duplicates, keeping high values
dyadmid = ddply(dyadmid, .(dyadidyr), summarize, 
                 hihost = max(hihost), 
                 recip = max(recip), 
                 fatlev = max(fatlev))



