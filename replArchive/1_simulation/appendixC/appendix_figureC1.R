#############################
# set a path
require(here)
pth = paste0(here::here(), '/')
graphicsPath=simResPath = pth

# install github from specific repo
# to get same results
# devtools::install_github('s7minhas/amen', ref='pa2018_version')
library(amen)

# source in helper functions
source(paste0(pth, '/helpers/functions.R'))

# load in additional pkgs
toLoad = c(
	'devtools',
	'foreach', 'doParallel',
	'magrittr', 'dplyr', 'ggplot2',
	'latex2exp', 'extrafont', 'Cairo'
	)
loadPkg(toLoad)
suppressMessages(loadfonts(device='win'))
facet_labeller = function(string){ TeX(string) }
##############################

##############################
# params
intEff=-2 ; x1Eff=1 ; x2Eff=1

# check if sim results are in path
fileLogical = !all( file.exists(
	paste0(simResPath,'ameSim',c(50,100),'_asaProbit.rda') ) )

# if they arent in path then run sim script
if( fileLogical ){
	source(paste0(pth, '1_simulation/appendixC/1_ameSim_asaProbit.R')) }
# load sim results
for(n in c( 50,100)){ load(paste0(simResPath,'ameSim',n,'_asaProbit.rda')) }

#
modKey = data.frame(dirty=names(ameSim50[[1]]$beta))
modKey$clean = c('Standard', 'AME', 'Oracle')
##############################

##############################
# check bias of x1
getBiasDF = function(ameSim){
	biasDF = lapply(ameSim, function(x){
		betaMod=lapply(x$beta, function(z){ apply(z, 2, median)[1:2] })
		betaMod = betaMod %>% reshape2::melt() %>%
			dplyr::mutate(
				var=rep(c('Intercept','X1'), length(x$beta)),
				act = rep(c(intEff, x1Eff), length(x$beta)),
				bias = value - act ) %>%
			dplyr::rename(model = L1)
		return(betaMod) })
	return( suppressMessages( reshape2::melt(biasDF,id=names(biasDF[[1]])) ) ) }

#
ameSimBias = rbind(
	cbind(getBiasDF(ameSim50), n=50),
	cbind(getBiasDF(ameSim100), n=100) )
##############################

##############################
# clean
ameSimBias = ameSimBias %>% group_by(model, var, n) %>%
	dplyr::mutate( mse = mean(bias^2) ) %>% data.frame()

# fix mod labels
ameSimBias$model = modKey$clean[match(ameSimBias$model, modKey$dirty)]
ameSimBias$model = factor(ameSimBias$model, levels=modKey$clean)
modCols = c(Standard='#d6604d', AME='#4393c3', Oracle='#4daf4a')

# fix var labels
ameSimBias$var[ameSimBias$var=='X1'] = '$\\beta$'
ameSimBias$var[ameSimBias$var=='Intercept'] = '$\\mu$'
ameSimBias$var = factor(ameSimBias$var, levels=c("$\\mu$", "$\\beta$"))

# viz
ggBiasPlot = function(varName, h=4, w=8){
	if(varName!='all'){
		g=ggplot(
			filter(ameSimBias, var==paste0('$\\',varName,'$')),
			aes(x=model, y=value, fill=model,color=model)) }
	if(varName=='all'){
		g=ggplot(ameSimBias, aes(x=model,y=value,fill=model,color=model)) }
	g = g +
		geom_hline(aes(yintercept=act), color='grey60', size=2) +
		geom_jitter(alpha=.1) +
		geom_boxplot( color='black') +
		facet_grid(var ~ n, scales='free_y',
			labeller=as_labeller(facet_labeller, default = label_parsed)) +
		xlab('') + ylab('') +
		scale_color_manual(values=modCols) +
		scale_fill_manual(values=modCols) +
		theme(
			legend.position='none',
			legend.title=element_blank(),
			axis.ticks=element_blank(),
			panel.border=element_blank(),
			axis.text.y=element_text(size=8
				#, family="Source Code Pro Light"
				),
			axis.text.x=element_text(size=10, face='bold'),
			strip.text.x = element_text(size=9, color='white'
				#, family="Source Code Pro Semibold"
				),
			strip.text.y = element_text(size=9, color='white',
				#family="Source Code Pro Semibold",
				angle=0),
			strip.background = element_rect(fill = "#525252", color='#525252')
			)
	ggsave(g, height=4, width=8,
		file=paste0(graphicsPath, 'appendix_figureC1.pdf')
		#, device=cairo_pdf
		)
	return(g)
}

#
ggBiasPlot('all', h=12, w=8)
##############################
