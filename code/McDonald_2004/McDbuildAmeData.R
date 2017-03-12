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

## correct for asymmetric actor list:

pds = sort(unique(data$year))
length(pds) ##44

## want actorList[[i]] to be about 20-30 unique

actorList = lapply(pds, function(t){
    slice = data[data$year==t,c('ccode1' , 'ccode2')]
    sort(unique(c(slice$ccode1, slice$ccode2)))
})

## remove ccodes 212, 369, and483
## each only available for one year
## Update 3/11: why do this?

## countriesT = lapply(countriesT, function(x){ x[x!='212'] })
## countriesT = lapply(countriesT, function(x){ x[x!='369'] })
## countriesT = lapply(countriesT, function(x){ x[x!='482'] })


#########################################
## Build list of adjacency matricies for the DV
## dv
#########################################3


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


#### Build list of dyadic variable values

## dyadic vars

baseVars ## all vars

baseVars[2:16] ## confirm that these are the ones  I want

dVars = baseVars[2:16]

#head(cbind(data$ccode1, data$ccode2, data[,dVars]))

xDyadList = lapply(1:length(years), function(ii){
    slice = data[ which( 
        data$year==years[ii] & 
        data$ccode1 %in% actorList[[ii]] & 
        data$ccode2 %in% actorList[[ii]]
        ), c('ccode1', 'ccode2', dVars) ]
    
    symmetry <- as.data.frame(
        ## all of the dependent variables
        ## are dyadic, so can just place the data
        ## in naively ...I think 
        
        cbind(slice$ccode2, slice$ccode1, slice[,dVars]))
    names(symmetry)=c("ccode1", "ccode2",dVars)
    
                                        #head(symmetry)
        ## bind the two to make a faux "directed"
        ## edgelist:
    dat <- rbind(slice, symmetry)
    
    
    ##check for asymmetries:
    ## just first dimension so doesn't throw warnings
    if(dim(dat)[1] !=dim(unique(dat))[1] ){
        print(paste0("Check for duplicated entries in year  ", ii))}
    
## now produce adjacency matrix
        
    sliceL = reshape2::melt(dat, id=c('ccode1','ccode2')) 
    adj = reshape2::acast(sliceL, ccode1 ~ ccode2 ~ variable,
        value.var='value')
    
    
    return(adj[char(actorList[[ii]]), char(actorList[[ii]]),  ]    )
}) ; names(xDyadList) = years

ls()
length(xDyadList)

###########################################
## Build nodal variables
## README: McDonald doesn't have any node-specific vars
## that are not just identifiers
###########################################

## nVars = c(..)

## xNodeList = lapply(1:length(years), function(ii){

##     slice = unique( data[ which( 
## 			data$year==years[ii] & 
## 			data$ccode1 %in% actorList[[ii]] & 
## 			data$ccode2 %in% actorList[[ii]]
##         ), c('ccode1', nVars) ] )
    
##     symmetry <- as.data.frame(
##         cbind(slice$ccode2, slice$ccode1,slice$cw2mid))
##     names(symmetry)=c("ccode1", "ccode2", nVars)
    
##     ## bind the two to make a faux "directed"
##     ## edgelist:
##     dat <- rbind(slice, symmetry)  
    
##     #if(nrow(slice)!=length(actorList[[ii]])){
##      #   stop('# rows dont match')
##     #}
    
##     #regionSplit = model.matrix(~region-1, data=slice)
##     adj = data.matrix(cbind( slice[,nVars[-length(nVars)]], regionSplit ))
## 	rownames(adj) = slice$ccode1
## 	return( adj[ char(actorList[[ii]]), ]  )
##     }) ; names(xNodeList) = years


##########################################
## Save data
###########################################

save(yList, xDyadList, file=paste0(dataPath, 'amenData.rda'))
