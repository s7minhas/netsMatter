

##### Wright and Diehl 2014 Replication #####



rm(list=ls())

library(foreign)
library(lmtest)
library(multiwayvcov)

load("/Users/jordan/Dropbox/netsMatter/replications/wrightdiehl/wdData.R")




##### Table 2, Model 3 #####

m1 = glm(cowwar ~ mixed + terrXmixed + territor + 
           jtdem + caprat + terrcount + rival, 
         data = data, 
         family = 'binomial')
summary(m1)

# Results: 
  # exact replication






