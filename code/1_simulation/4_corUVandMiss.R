##############################
rm(list=ls())
if(Sys.info()['user']=='s7m'){
	fPath = '~/Research/netsMatter/code/helpers/'
	dPath = '~/Dropbox/Research/netsMatter/'
	simResPath = paste0(dPath, 'simulation/')
	graphicsPath = paste('~/Research/netsMatter/paper/graphics/')
	source(paste0(fPath, 'functions.R')) }

if(Sys.info()['user'] %in% c('herme','Owner','S7M')){
	base=paste0('C:/Users/',Sys.info()['user'],'/')
	gPath = paste0(base, 'Research/netsMatter/')
	fPath = paste0(gPath, 'code/helpers/')
	dPath = paste0(base, 'Dropbox/Research/netsMatter/')
	simResPath = paste0(dPath, 'simulation/')
	graphicsPath = paste0(gPath, 'paper/graphics/')
	source(paste0(fPath, 'functions.R')) }

toLoad = c(
	'devtools',
	'foreach', 'doParallel',
	'magrittr', 'dplyr', 'ggplot2',
	'latex2exp', 'Cairo', 'extrafont'
	)
loadPkg(toLoad)
suppressMessages(loadfonts())
facet_labeller = function(string){ TeX(string) }
##############################

##############################
# params
NSIM = 1000 ; intEff=-2 ; x1Eff=1 ; x2Eff=1

# load sim results
for(n in c( 50,100)){ load(paste0(simResPath,'ameSim',n,'.rda')) }
##############################

##############################
# check that uv covers missvar
# missing var
genMissVar = function(n, imps=NSIM){
	lapply(1:imps, function(seed){ n=n ;
		set.seed(seed) ; xw = matrix(rnorm(n*2),n,2)
		W = tcrossprod(xw[,2]) ; return(W) }) }

#
getCor = function(ameSim, W){
	cor = lapply(1:length(ameSim), function(i){
		uv = c(ameSim[[i]]$uv$ame)
		w = c(W[[i]])
		cor(uv, w, use='pairwise.complete.obs') })
	return(unlist(cor)) }

#
ameSimCor = rbind(
	cbind(getCor(ameSim50, genMissVar(50)), n=50),
	cbind(getCor(ameSim100, genMissVar(100)), n=100)
	) %>% data.frame()
ameSimCor$n = paste0('n = ',ameSimCor$n)
ameSimCor$n = factor(ameSimCor$n, levels=paste0('n = ',c(100,50)))

loadPkg('plyr')
ameSimCorMeans = ddply(ameSimCor, .(n), summarise, sMedian=median(V1))
ameSimCorDensity = ddply(ameSimCor, .(n), .fun=function(x){
	tmp = density(x$V1); x1 = tmp$x; y1 = tmp$y
	q95 = x1 >= quantile(x$V1,0.025) & x1 <= quantile(x$V1,0.975)
	q90 = x1 >= quantile(x$V1,0.05) & x1 <= quantile(x$V1,0.95)
	data.frame(x=x1,y=y1,q95=q95, q90=q90) } )
##############################

##############################
# color
col = '#4393c3'

# cleanup
ameSimCorMeans$sMedianLab = paste0('Median Correlation: ', round(ameSimCorMeans$sMedian,3))
ameSimCorMeans$y[ameSimCorMeans$n=='n = 100'] = 45
ameSimCorMeans$y[ameSimCorMeans$n=='n = 50'] = 20

# viz
g = ggplot() +
	geom_line(data=ameSimCorDensity, aes(x=x,y=y)) +
	geom_ribbon(data=subset(ameSimCorDensity,q95),
		aes(x=x,ymax=y),ymin=0,alpha=0.5) +
	geom_ribbon(data=subset(ameSimCorDensity,q90),
		aes(x=x,ymax=y),ymin=0,alpha=0.9) +
	geom_vline(data=ameSimCorMeans,
		aes(xintercept=sMedian),linetype='solid',size=1, color=col) +
	geom_text(data=ameSimCorMeans,
		aes(label=sMedianLab,x=sMedian,y=y), hjust=1.05, size=2.5, fontface='bold', color='black') +
	facet_grid(n~.) +
	xlab('') + ylab('') +
	theme(
		axis.ticks=element_blank(),
		panel.border=element_blank(),
		axis.text.y=element_blank(),
		strip.text.x = element_text(size=9, color='white'),
		strip.text.y = element_text(size=9, color='white'),
		strip.background = element_rect(fill = "#525252", color='#525252')
		)

#
ggsave(g, height=3, width=8,
	file=paste0(graphicsPath, 'ameSimCorr.pdf'))
##############################
