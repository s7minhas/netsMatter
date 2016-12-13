# 
#rm(list=ls())

load('/Users/juanftellez/Dropbox/netsMatter/replications/Reiter_Stam_2003/output/amenData.rda')


dyadLong = reshape2::melt(xDyadList)
dyadLong = dyadLong[dyadLong$Var1 != dyadLong$Var2 , ]
dyadLong$Var3 = as.character(dyadLong$Var3)
dyadLong$Var1 = as.character(dyadLong$Var1)
dyadLong$Var2 = as.character(dyadLong$Var2)

varLong = dyadLong[dyadLong$Var3=='loglsrat',]
summary(varLong)


# which missing
varMiss = varLong[is.na(varLong$value), ]
varMiss$Var1 = as.character(varMiss$Var1)
varMiss$Var2 = as.character(varMiss$Var2)
varMiss$year = yrs[varMiss$L1]
sort(table(c(varMiss$Var1, varMiss$Var2)))

# exclude: 581, 692, 110, 352
View(varMiss[(varMiss$Var1 %in% c(581) |
                 varMiss$Var2 %in% c(581)), ])
