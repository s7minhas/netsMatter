## This file loads packages needed to do the replications and run AMEN
## and also identifies paths to load and save data
## the script is read into the start of future files

## if on Dave's server

if(Sys.info()['user']== 'margaret' | Sys.info()['user']== 'root' ){
    dataPath='~/projects/netsmatter/data/'
    dPath='./results/'
    gPath='~/projects/netsmatter/code/netsMatter/code/McDonald_2004/'                                          #graphicPath='/results/' # path to dir where i will store any graphics
                                        #resultsPath='/results/' # path to dir where i will store results
    funcPath=paste0(gPath, 'code/helpers/') # helpers directory in ~/Research
}



## If on one of my machines
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
    dataPath='~/Dropbox/netsMatter/replications/Weeks2012'## where the Weeks data lives
    dPath='~/Dropbox/netsMatter/' # general dropox path
    gPath='~/Research/netsMatter/' # path to github in case i need to call in helper functions
    graphicsPath=paste0(dPath, 'replications/Weeks2012/graphics/') # path to dir where i will store any graphics
    resultsPath=paste0(dPath, 'replications/Weeks2012/replication/output/') # path to dir where i will store results
    funcPath=paste0(gPath, 'code/helpers/') # helpers directory in ~/Research

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
    'ggplot2', 'latex2exp', 'Cairo',# plotting
    'xtable', # tables
    'devtools', # loading git packages
    'gridExtra', #for the param plots
    'doParallel','foreach' #packages for parallelization
    ))

print("packages loaded")
## load amen

## for github install:
                                        #devtools::install_github('s7minhas/amen') ; library(amen)

## for my local clone:

if(Sys.info()['user']== 'margaret' | Sys.info()['user']== 'root'){
    print("Loading local clone of AMEN")
    library(devtools)
    document('~/projects/netsmatter/amen/')
    install('~/projects/netsmatter/amen/')
    library(amen)
}

## if on one of my macs
if(Sys.info()['user']=='algauros' | Sys.info()['user']=='Promachos'){
    print("loading AMEN from Github")
    devtools::install <- github('s7minhas/amen') ; library(amen)
}

                                        # Set a theme for gg
theme <- set(theme <- bw())

                                        # misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

#print(sessionInfo())

## AMen discussion from class

## red is observed, the histogram is wha we should see;
##dyad.dep is looking for structure in the residuals (to see if there is any structure)

## triad.depednence is proportional to number of triangles / number of possible triangles


##if triad.dependence doesn't look good: how woudl we go about capturing. Someone might do so
## but the structural element is hard to interpret. Could condition on number of triangles, which means a measure of how close
## triads are
## Number of triangles: Sum(EEE)/[Sum((11t) (11t) (11t) ] where E is
## the error matrix.
## really a summary of how close things are when you multiply the
## Error matrix three times.

## Try to introduce some other thing that can capture triadic
## behavior: here is latent space.


## SRRF has  poor behavior for "triadic behavior"
## Can capture information like homophily or stoachstic equilvanece 3


## Latent Space Model
##motivated by Aldous -Hoover + Lovass- Bolbas graph limit

## latent space model is much more general; sequence of papers on the
## website as well. 
