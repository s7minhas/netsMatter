
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
modData = na.omit( weeksData[,c(dv,ivs,other,ids)] )

## Start AMEN build process


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

difs2


## all zeros, so seems ok:
difs


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



## Next up:
## Figure out which are dyadic variables

## and which are the nodal variables

## then run dvars and nvars
