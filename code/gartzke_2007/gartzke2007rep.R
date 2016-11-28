
##### Gartzke 2007 Replication #####



rm(list=ls())

library(foreign)
library(lmtest)
library(multiwayvcov)

data = read.dta("/Users/jordan/Dropbox/netsMatter/replications/gartzke2007/capitalistpeace_012007.dta")


# rename the spline variables to avoid issues with R syntax
colnames(data)[34] = 'spline1'
colnames(data)[35] = 'spline2'
colnames(data)[36] = 'spline3'





##### Table 1, Model 5 #####

m1 = glm(maoznewl ~ demlo + demhi + deplo +
             capopenl + rgdppclo + gdpcontg + 
             sun2cati + contig + logdstab +
             majpdyds + alliesr + lncaprt +
             spline1 + spline2 + spline3, 
           data = data, 
           family = 'binomial')
summary(m1)

### Cluster SE's by dyad

# Shahryar's solution
baseModelVcov = cluster.vcov(model=m1, cluster=data$dyadid, 
                             df_correction = FALSE, leverage = 3)
m1cluster = coeftest(m1, baseModelVcov)



m1cluster
# Results: 
  # coefficients replicate perfectly
  # SE's are very close or identical after rounding
    # just different because of clustering algorithms? 



 

