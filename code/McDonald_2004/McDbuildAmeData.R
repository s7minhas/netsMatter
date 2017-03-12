## This script to generate data in the format that AME needs

## reminder of the model: free trade promotes peace more than protectionist trade
## indicators of openess:
## ratio of a country’s customs duties to its total imports
## protection levels

## original unit of analysis: dyad-year
## DV (MIDON) is oneet of new MID between i and j.

## paths
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
 dataPath='~/Dropbox/netsMatter/replications/McDonald_2004/data/'
}


char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }

trim = function (x) { gsub("^\\s+|\\s+$", "", x) }


# load data
data <- foreign::read.dta(paste0(dataPath, 'PTTOFTfvs.dta'))

class(data)
dim(data) ##534274 x 23

head(data)

baseVars = c('cw2mid', 'cw2midspl','cw2midsp1',
    'cw2midsp2', 'cw2midsp3', 'ally','cont1', 'lncaprat','ldep2l',
    'grow61l', 'lpolity42l','s_wt_glo', 'lrgdpch61h', 'lndistan',
    'majpow', 'limpduty0200h','ccode1', 'ccode2', 'year', 'dyadid')

## adding in a MID ID variable:

data$midid <- 1:nrow(data)
head(data)


## cut down to  sampling frame
data = na.omit(data[, baseVars])

dim(data) #87180X 20
head(data)

years = sort(unique(data$year)) #1971-2000


## countriesT = lapply(years, function(t){
##     as.character(unique(data$ccode1[data$year==t]) ) })

## #countriesT
## # get count of actors by year
## tail(sort(table( unlist( countriesT ) )))

######################
## Check for symmetry

## mst = lapply(years, function(t){
##    unique(data$ccode1[data$year==t])})


## mst2 = lapply(years, function(t){
##    unique(data$ccode2[data$year==t])})

## difs <- lapply(1:length(years), function(y){
##     ##
##     setdiff(mst2[[y]], mst[[y]]) 
## })

## difs2 <- lapply(1:length(years), function(y){
##     ##
##     setdiff(mst[[y]], mst2[[y]]) 
## })


## difs ## fails test!
## difs2

#######

## correct for asymmetric actor list:

pds = sort(unique(data$year))
length(pds) ##44

actorList = lapply(pds, function(t){
    slice = data[data$year==t,c('ccode1' , 'ccode2')]
    sort(unique(c(slice$ccode1, slice$ccode2)))
})

## investigate what actorList produces:
class(actorList) #list
length(actorList) ## 44 (so, each year)
class(actorList[[1]]) #numeric

length(actorList[[1]]) #want this to be 20-30, depending on i
length(actorList[[2]])

## remove ccodes 212, 369, and483
## each only available for one year
## Update 3/11: why do this?

## countriesT = lapply(countriesT, function(x){ x[x!='212'] })
## countriesT = lapply(countriesT, function(x){ x[x!='369'] })
## countriesT = lapply(countriesT, function(x){ x[x!='482'] })

## dv

## to here: need to find out where actorList goes.

yVar = 'cw2mid'

yList = lapply(1:length(years), function(ii){
    slice = data[ which( 
        data$year==years[ii] & 
        data$ccode1 %in% actorList[[ii]] & 
        data$ccode2 %in% actorList[[ii]]
        ), c('ccode1', 'ccode2', yVar) ]

    ## this creates the symmetric adj matrix
    symmetry <- as.data.frame(
        cbind(slice$ccode2, slice$ccode1,slice$cw2mid))
        names(symmetry)=c("ccode1", "ccode2", "cw2mid")

        ## bind the two to make a faux "directed"
        ## edgelist:
        dat <- rbind(slice, symmetry)  
        
        ## now produce adjacency matrix
        adj = reshape2::acast(dat, ccode1 ~ ccode2, value.var=yVar)

        ## as character so that it reads the actor list
        ## as row/col names
        tmp <- adj[char(actorList[[ii]]), char(actorList[[ii]])]

        ##send out and close apply:
        return(tmp)}) ; names(yList) = years

length(yList)
yList[[1]]

## year 1= 1971, which happened to have no MIDs
## in this data
rowSums(yList[[1]], na.rm=TRUE)

years[15] #1985, should have 32
table(rowSums(yList[[15]], na.rm=TRUE))

## dyadic vars

dVars = c(...)

xDyadList = lapply(1:length(years), function(ii){
	slice = data[ which( 
			data$year==years[ii] & 
			data$ccode1 %in% actorList[[ii]] & 
			data$ccode2 %in% actorList[[ii]]
			), c('ccode1', 'ccode2', dVars) ]
	sliceL = reshape2::melt(slice, id=c('ccode1','ccode2'))
	adj = reshape2::acast(sliceL, ccode1 ~ ccode2 ~ variable, value.var='value')
	return( adj[ actorList[[ii]], actorList[[ii]],  ] )
}) ; names(xDyadList) = years

# nodal vars
nVars = c(..)

xNodeList = lapply(1:length(years), function(ii){
	slice = unique( data[ which( 
			data$year==years[ii] & 
			data$ccode1 %in% actorList[[ii]] & 
			data$ccode2 %in% actorList[[ii]]
			), c('ccode1', nVars) ] )
	if(nrow(slice)!=length(actorList[[ii]])){
            stop('# rows dont match')
        }
        
	regionSplit = model.matrix(~region-1, data=slice)
	adj = data.matrix(cbind( slice[,nVars[-length(nVars)]], regionSplit ))
	rownames(adj) = slice$ccode1
	return( adj[ actorList[[ii]], ]  )
    }) ; names(xNodeList) = years

# save dfs
save(yList, xDyadList, xNodeList, file=paste0(dataPath, 'amenData.rda'))
