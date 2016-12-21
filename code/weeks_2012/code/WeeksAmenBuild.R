
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
 pathData='~/Dropbox/netsMatter/replications/Weeks2012/replication/data/'
}

## libraries

loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
	}
}

loadPkg(c('foreign', 'lmtest', 'sandwich'))

## load full data
weeksData = foreign::read.dta(paste0(pathData, 'WeeksAPSR2012.dta'))

## subset to relev covars
dv = 'mzinit'
ivs = c(
	'machinejlw_1', 'juntajlw_1', 'bossjlw_1', 'strongmanjlw_1',
	'allotherauts_1', 'newregime_1', 'democracy_2', 'cap_1', 'cap_2',
	'initshare', 'dependlow', 'majmaj', 'minmaj', 'majmin', 'contigdum',
	'logdist', 's_wt_glo', 's_lead_1', 's_lead_2', 'pcyrsmzinit',
	'pcyrsmzinits1', 'pcyrsmzinits2', 'pcyrsmzinits3'
	)
other = 'democracy_1'
ids = c('ccode1', 'ccode2', 'year', 'dirdyadid')

## construct data for modelling
modData = na.omit(weeksData[,c(dv,ivs,other,ids)] )


###########################
## Sort vars into nodal or dyadic variables:
##########################

## Classifying the variables:

#### Dyadic variables: 
## "dependlow"     
##"majmaj", "minmaj" "majmin", "minmin" = major powers. (Order codes for node-level inforamation about which state is the major power)
## "contigdum" = whether states share a land border
## "logdist" = log distance between capitals
## "s_wt_glo" = similarity of alliance porfolio
##"pcyrsmzinit"    "pcyrsmzinits1"  "pcyrsmzinits2"  "pcyrsmzinits3"
## I bet these are the cubic splies for previous year since State A initiated against state B

dyadicVars <- c( "dependlow" , "majmaj", "minmaj", "majmin", "contigdum",
                "logdist","s_wt_glo","pcyrsmzinit", "pcyrsmzinits1", "pcyrsmzinits2",
                "pcyrsmzinits3")
###############
#### node-level
################

## regime type of initator or reciever: 
## "machinejlw_1"   "juntajlw_1"     "bossjlw_1"     
## "strongmanjlw_1" "allotherauts_1" "newregime_1"    "democracy_2"

## "cap_1", "cap_2"  = raw capabilites of each side
## "initshare" = initiator's share of dyad's total capabilities
## "s_lead_1","s_lead_2" = each state's similarity to most powerful state in system
##"democracy_2" = whether side B is a democracy

## Dyad-level identifiers
## "ccode1"         "ccode2"  = identifiers


nodalVars.i <- c("machinejlw_1", "juntajlw_1", "bossjlw_1",
                  "strongmanjlw_1", "allotherauts_1", "newregime_1",  "cap_1",
                 "s_lead_1" )

nodalVars.r <- c("democracy_2" , "cap_2", "s_lead_2")



#####################################
## Start AMEN build process
######################################

years <- sort(unique(modData$year)) ## 1950-1999

## Lists first country in the pair, with an entry for each year: 
cntriesT = lapply(years, function(t){
   unique(modData$ccode1[modData$year==t])})


###############################################
## Do I miss any countries by only looking at
## ccode1?
##############################################
## want setdiff(ccode2[year i], ccode1[year i])
## take setdiff() in both directions, b/c
## outcome is not symmetrical
## (eg: setdiff(1:5, 3:6) != setdiff(3:6, 1:5))

mst = lapply(years, function(t){
   unique(modData$ccode1[modData$year==t])})


mst2 = lapply(years, function(t){
   unique(modData$ccode2[modData$year==t])})

difs <- lapply(1:length(years), function(y){
    ##
    setdiff(mst2[[y]], mst[[y]]) 
})

difs2 <- lapply(1:length(years), function(y){
    ##
    setdiff(mst[[y]], mst2[[y]]) 
})

## all zeros, so seems ok:

## checks to ensure that the difference lists are
## of length 0 for the entire list:
checklist <- function(list){
    lapply(list, function(x) ifelse(length(x)==0,
                                 print("ok"),
                                 print("Warning! Not Symmetrical!")))

}

checklist(difs2)
checklist(difs)

## Lists of the DV:

yList = lapply(1:length(years), function(ii){
	slice = modData[which( 
			modData$year==years[ii] & ## year
			modData$ccode1 %in% cntriesT[[ii]] & ## country A
			modData$ccode2 %in% cntriesT[[ii]] ## country b
            ), c('ccode1', 'ccode2', dv) ]
        ##acast returns a vector, matrix, or array
        ## synax is: takes slice, ccode1 as id, ccode2 as the y var,
        ## value var is the name of the variable that stores the values
	adj = reshape2::acast(slice, ccode1 ~ ccode2, value.var=dv)
        ## adj will produce a matrix, and this wants only those
        ## rows and columns in the list of countries for each year

        ## original problem in subsetting is that the codes need to be characters
        ## not numbers to match the names of the columns and rows
        includedCountries <- as.character(cntriesT[[ii]])
	return(adj[includedCountries, includedCountries])
    })


xDyadList = lapply(1:length(years), function(ii){
	slice = modData[which( 
			modData$year==years[ii] & 
			modData$ccode1 %in% cntriesT[[ii]] & 
			modData$ccode2 %in% cntriesT[[ii]]
			), c('ccode1', 'ccode2', dyadicVars) ]
	sliceL = reshape2::melt(slice, id=c('ccode1','ccode2'))
	adj = reshape2::acast(sliceL, ccode1 ~ ccode2 ~ variable, value.var='value')
        print(dim(adj))
        includedCountries <- as.character(cntriesT[[ii]])
        return(adj[includedCountries, includedCountries,]) #last comma is impt: gets the third dimension
	##return(adj)
    })


## node list:

nVars <- c(nodalVars.r, nodalVars.i)

## concatenating the node and 
xNodeList = lapply(1:length(years), function(ii){
	slice = unique( modData[which( 
			modData$year==years[ii] & 
			modData$ccode1 %in% cntriesT[[ii]] & 
			modData$ccode2 %in% cntriesT[[ii]]
            ), c('ccode1', nVars) ] )

        print(nrow(slice))
        print(length(cntriesT[[ii]]))
	if(nrow(slice)!=length(cntriesT[[ii]])){ stop('# rows dont match')  }
	regionSplit = model.matrix(~region-1, data=slice)
	adj = data.matrix(cbind( slice[,nVars[-length(nVars)]], regionSplit ))
	rownames(adj) = slice$ccode1
	return( adj[ cntriesT[[ii]], ]  )
})

## walk through:

slice.tmp = unique(modData[which( 
    modData$year==years[1] & 
    modData$ccode1 %in% cntriesT[[1]] & 
    modData$ccode2 %in% cntriesT[[1]]
    ), c('ccode1', nVars) ] )

class(slice.tmp)
dim(slice.tmp)

dim(modData[which(modData$year==years[[1]]),]) #5256

head(slice.tmp)

