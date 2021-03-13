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
	'ggplot2', 'latex2exp', 'Cairo', 'RColorBrewer',	# plotting
	'xtable', # tables
	'countrycode',
	'reshape2', 'tidyr', 'stringr',	'dplyr'
	))

# Set a theme for gg
theme_set(theme_bw())

# misc
pasteVec = function(x,y){ as.vector( outer( x, y, paste0 ) ) }
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
