if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ){
	dPath='~/Dropbox/Research/amenRepl/'
	dataPath=paste0(dPath, 'data/mansfield_milner_2012/')
	graphicsPath=paste0(dPath, 'graphics/mansfield_milner_2012/')
	resultsPath=paste0(dPath, 'results/mansfield_milner_2012/')
	gPath='~/Research/amenRepl/'
}

# install/load libraries
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
	}
}

# some necessary libs
loadPkg(c(
	'foreign',
	'reshape2', # data management
	'ggplot2', 'latex2exp', 'Cairo',	# plotting
	'xtable', # tables
	'devtools' # loading git packages
	))

# load amen
devtools::install_github('s7minhas/amen') ; library(amen)

# Set a theme for gg
theme_set(theme_bw())

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }