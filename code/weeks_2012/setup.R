# This file loads packages needed to do the replications and run AMEN
## and also identifies paths to load and save data
## the script is read into the start of future files

## if on Dave's server
if(Sys.info()['user']== 'margaret' | Sys.info()['user']== 'root' ){
    dataPath='~/projects/netsmatter/data/'
    dPath='./results/'
    gPath='~/projects/netsmatter/code/netsMatter/code/weeks_2012/'                                          #graphicPath='/results/' # path to dir where i will store any graphics
    #resultsPath='/results/' # path to dir where i will store results
    funcPath=paste0(gPath, 'code/helpers/') # helpers directory in ~/Research
}



## If on one of my machines
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
    dataPath='~/Dropbox/netsMatter/replications/Weeks2012/replication/output/'## where to send results
    inputPath= '~/Dropbox/netsMatter/replications/Weeks2012/replication/input/' ## Where the raw and AMEN formatted data live
    dPath='~/Dropbox/netsMatter/' # general dropox path
    graphicsPath=paste0(dPath, 'replications/Weeks2012/graphics/') # path to dir where i will store any graphics
    resultsPath=paste0(dPath, 'replications/Weeks2012/replication/output/') # path to dir where i will store results
    funcPath='./helpers/' # helpers directory in ~/Research
    
}

## need to declare these, based on the system-specific dPath and gPath


# install/load libraries
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
	}
}

## some necessary libs
loadPkg(c(
    'foreign','roxygen2',
    'dplyr', "Rcpp", "RcppArmadillo", "plyr",
    'reshape2', # data management
    'ggplot2', 'latex2exp', 'Cairo',	# plotting
    'xtable', # tables
    'devtools', # loading git packages
    'gridExtra', #for the param plots
    'doParallel','foreach' #packages for parallelization
	))

print("packages loaded")
                                        ## load amen

## for github install:
devtools::install_github('s7minhas/amen', ref = 'dev') ; library(amen)

## for my local clone:

## if(Sys.info()['user']== 'margaret'  | Sys.info()['user']== 'root'){
##     print("Loading local clone of AMEN")
##     library(devtools)
##     document('~/projects/netsmatter/amen/')
##     install('~/projects/netsmatter/amen/')
## }

## if on one of my macs
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
    print("loading AMEN from Github")
    devtools::install <- github('s7minhas/amen') ; library(amen)
}

# Set a theme for gg
theme_set(theme_bw())

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

print(sessionInfo())
