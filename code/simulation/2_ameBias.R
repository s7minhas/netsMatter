##############################
rm(list=ls())
if(Sys.info()['user']=='s7m'){
	fPath = '~/Research/netsMatter/code/helpers/'
	dPath = '~/Dropbox/Research/netsMatter/'
	simResPath = paste0(dPath, 'simulation/')
	graphicsPath = paste('~/Research/netsMatter/paper/')
	source(paste0(fPath, 'functions.R')) }

toLoad = c(
	'devtools', 
	'foreach', 'doParallel',
	'magrittr', 'dplyr', 'ggplot2',
	'latex2exp', 'Cairo'
	)
loadPkg(toLoad)
facet_labeller = function(string){ TeX(string) }
##############################

##############################
# params
NSIM = 1000 ; intEff=-2 ; x1Eff=1 ; x2Eff=1

# load sim results
for(n in c( 50,100)){ load(paste0(simResPath,'ameSim',n,'.rda')) }

#
modKey = data.frame(dirty=names(ameSim50[[1]]$beta))
modKey$clean = c('Naive', 'AME', 'Oracle')
##############################

##############################
# check bias of x1
getBiasDF = function(ameSim){
	biasDF = lapply(ameSim, function(x){
		betaMod=lapply(x$beta, function(z){ apply(z, 2, median)[1:2] })
		betaMod = betaMod %>% reshape2::melt() %>%
			mutate(
				var=rep(c('Intercept','X1'), length(x$beta)),
				act = rep(c(intEff, x1Eff), length(x$beta)),
				bias = value - act ) %>%
			rename(model = L1)
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
	mutate( mse = mean(bias^2) ) %>% data.frame()
ameSimBias$model = modKey$clean[match(ameSimBias$model, modKey$dirty)]
ameSimBias$model = factor(ameSimBias$model, levels=modKey$clean)
ameSimBias$var[ameSimBias$var=='X1'] = '$\\beta$'
ameSimBias$var[ameSimBias$var=='Intercept'] = '$\\mu$'

ggBiasPlot = function(varName){
	g=ggplot(filter(ameSimBias, var==paste0('$\\',varName,'$')),
			aes(x=model, y=value, fill=model,color=model)) +
		geom_hline(aes(yintercept=act), color='grey60', size=2) +	
		geom_jitter(alpha=.1) +
		geom_boxplot( color='black') + 	
		# geom_violin(
		# 	draw_quantiles=c(0.025,0.5,0.975),
		# 	trim=TRUE, color='black', alpha=.4) +	
		facet_grid(var ~ n, scales='free_y',
			labeller=as_labeller(facet_labeller, default = label_parsed)) + 
		xlab('') + ylab('') +
		theme(
			legend.position='top',
			legend.title=element_blank(),
			axis.ticks=element_blank(),
			panel.border=element_blank(),
			axis.text=element_text(size=8
				# , family="Source Code Pro Light")
			),
			strip.text.x = element_text(size=9, color='white'
				# ,family="Source Code Pro Semibold"
				),
			strip.text.y = element_text(size=9, color='white' 
				# ,family="Source Code Pro Semibold",
				,angle=0
				),		
			strip.background = element_rect(fill = "#525252", color='#525252')	
			)
	ggsave(g, height=4, width=8,
		file=paste0(graphicsPath, 'ameSimBias_',varName,'.pdf')
		# , device=cairo_pdf
		) }

#
ggBiasPlot('beta') ; ggBiasPlot('mu')
##############################