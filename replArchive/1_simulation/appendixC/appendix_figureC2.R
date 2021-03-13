#############################
# set a path
require(here)
pth = paste0(here::here(), '/replArchive/')
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
NSIM = 1000 ; intEff=-2 ; x1Eff=1 ; x2Eff=1

# sim labs
for(n in c( 50,100)){ load(paste0(simResPath,'ameSim',n,'_asaProbit.rda')) }

#
for(sim in 1:length(ameSim50)){
	names(ameSim50[[sim]]$'beta')[2] = 'ame' }
for(sim in 1:length(ameSim100)){
	names(ameSim100[[sim]]$'beta')[2] = 'ame' }

#
modKey = data.frame(dirty=names(ameSim50[[1]]$beta))
modKey$clean = c('Standard', 'AME', 'Oracle')
##############################

##############################
getCoverage = function(ameSim, model, n, varName, varNum, actual){
	intSumm = data.frame( do.call('rbind', lapply(ameSim, function(x){
		quantile(x$beta[[model]][,varNum], probs=c(0.025, 0.975))
	}) ) )
	intSumm$coverage = ( intSumm[,1]<actual & actual<intSumm[,2] )
	intSumm$model = model
	intSumm$n = n
	intSumm$varName = varName
	intSumm$actual = actual
	return(intSumm)
}

#
cover50x1 = do.call('rbind',lapply(c('naive','ame','oracle'), function(m){
	getCoverage(ameSim50, model=m, n=50, varName='X1', varNum=2, actual=x1Eff) }))
cover100x1 = do.call('rbind',lapply(c('naive','ame','oracle'), function(m){
	getCoverage(ameSim100, model=m, n=100, varName='X1', varNum=2, actual=x1Eff) }))
cover50int = do.call('rbind',lapply(c('naive','ame','oracle'), function(m){
	getCoverage(ameSim50, model=m, n=50, varName='Intercept', varNum=1, actual=intEff) }))
cover100int = do.call('rbind',lapply(c('naive','ame','oracle'), function(m){
	getCoverage(ameSim100, model=m, n=100, varName='Intercept', varNum=1, actual=intEff) }))

#
ameSimCover = data.frame( rbind(
	cover50x1, cover100x1, cover50int, cover100int ))
rm(list=c(
	paste0('ameSim',c(100, 50)),
	pasteVec(c( 'cover50','cover100'),c('x1','int'))) )

# summ by model
coverSumm = ameSimCover %>% group_by(model,n, varName) %>%
	dplyr::summarise(coverage=mean(coverage)) %>% data.frame()
##############################

##############################
# clean mod labels
coverSumm$model = modKey$clean[match(coverSumm$model, modKey$dirty)]
coverSumm$model = factor(coverSumm$model, levels=modKey$clean)
modCols = c(Standard='#d6604d', AME='#4393c3', Oracle='#4daf4a')

# clean var labels
coverSumm$varName[coverSumm$varName=='X1'] = '$\\beta$'
coverSumm$varName[coverSumm$varName=='Intercept'] = '$\\mu$'
coverSumm$varName = factor(coverSumm$varName, levels=c("$\\mu$", "$\\beta$"))

# coverage stat
coverSumm$covLab = paste0(round(coverSumm$coverage,3)*100,'%')
coverSumm$covLabY = ifelse(coverSumm$coverage>.9,.95,coverSumm$coverage+.03)

# viz
ggCoverPlot = function(var,h=3, w=8){
	if(var!='all'){
		g=ggplot(
			filter(coverSumm, varName==paste0('$\\',var,'$')),
			aes(x=model, y=coverage, fill=model,color=model)) }
	if(var=='all'){
		g=ggplot(coverSumm, aes(x=model, y=coverage, fill=model,color=model)) }
	g = g +
		geom_hline(aes(yintercept=.95), color='grey60', size=4, alpha=.5) +
		geom_linerange(aes(ymin=0, ymax=coverage),size=1.25) +
		geom_point(size=2) +
		facet_grid(varName~n, scales='free_y',
			labeller=as_labeller(facet_labeller, default = label_parsed)) +
		xlab('') +
		scale_color_manual(values=modCols) +
		scale_fill_manual(values=modCols) +
		scale_y_continuous('',
			breaks=seq(0,1.2,.2), labels=seq(0,1.2,.2), limits=c(0,1)) +
		annotate('text',
			x=1, y=.95, label='95% CI', color='black', size=2.5, fontface='bold') +
		geom_text(
			aes(label=covLab, y=covLabY), hjust=-.3, size=2.5, fontface='bold', color='black') +
		theme(
			legend.position='none',
			legend.title=element_blank(),
			axis.ticks=element_blank(),
			panel.border=element_blank(),
			axis.text.y=element_text(size=8,
				family="Source Code Pro Light"),
			axis.text.x=element_text(size=10, face='bold'),
			strip.text.x = element_text(size=9, color='white',
				family="Source Code Pro Semibold"),
			strip.text.y = element_text(size=9, color='white',
				family="Source Code Pro Semibold", angle=0),
			strip.background = element_rect(fill = "#525252", color='#525252')
			)
	ggsave(g, height=3, width=8,
		file=paste0(graphicsPath, 'appendix_figureC2.pdf'),
		device=cairo_pdf
		)
	return(g)
}

ggCoverPlot('all', h=6, w=8)
##############################
