#############################
# set a path
require(here)
pth = paste0(here::here(), '/')
rsPth = paste0(pth, '2_applications/application_data/reiter_stam/')

#
source(paste0(pth, 'helpers/functions.R'))
source(paste0(pth, 'helpers/ameHelpers.R'))

#
library(amen)
##############################

##############################
# load models

# if ame model for reiter and stam is not present run script
if(!file.exists(paste0(rsPth, 'ameFitReiterStam.rda'))){
  source(paste0(pth, '2_applications/reiter_stam_ameRun.R')) }

  # load in ame results
load(paste0(rsPth,'ameFitReiterStam.rda'))
##############################

# addEffdata ###########################################
# sender effects
effdat = getAddEffData(fit = ameFit) ##This function is in helperEx.R
effdat$cown = effdat$actor
effdat$actor = countrycode::countrycode(effdat$actor, 'cown', 'country.name')
effdat$actor = factor(effdat$actor,  levels=effdat[order(effdat$addEff),'actor'])

## spatial visualization
# fix actor names
cntryKey = data.frame(code=effdat$cown, cname=char(effdat$actor),stringsAsFactors = FALSE)
cntryKey$cname[cntryKey$code==731]='North Korea'
cntryKey$cowc = countrycode(cntryKey$cname, 'country.name', 'cowc')

# geo colors for nodes
loadPkg('cshapes')
cmap = cshp(date=as.Date('2016-1-1'))
cmapDF=fortify(cmap,region='FEATUREID')
names(cmapDF)[6]='FEATUREID'
cmapDF=join(cmapDF, cmap@data)
cmapDF$addEff = effdat$addEff[match(cmapDF$COWCODE, effdat$cown)]

ggMap = ggplot() +
  geom_polygon(data=cmapDF, aes(x=long, y=lat,group=group,fill=addEff),color='grey30',size=.05) +
  scale_fill_gradient2() +
  coord_equal() + xlab('') + ylab('') +
  labs(
    x='', y='', fill='Sender Effects'
    ) +
  theme(
    legend.position = 'bottom',
    legend.key.width = unit(1.25,"cm"),
    panel.border = element_blank(), panel.grid=element_blank(),
    axis.ticks = element_blank(), axis.line=element_blank(),
    axis.text = element_blank() )
ggsave(ggMap, file=paste0(pth, 'figure7a.pdf'), width=6, height=3)
system(paste('pdfcrop',
  paste0(pth, 'figure7a.pdf'),
  paste0(pth, 'figure7a.pdf')))

# pick out top countries
effdatSub = rbind(
  effdat[order(effdat$addEff, decreasing=TRUE),][1:10,],
  effdat[order(effdat$addEff),][1:10,]
  ) %>% na.omit()

# clean up names
effdatSub$actor = char(effdatSub$actor)
effdatSub$actor = cntryKey$cowc[match(effdatSub$actor, cntryKey$cname)]
effdatSub$actor = factor(effdatSub$actor,  levels=effdatSub[order(effdatSub$addEff),'actor'])

# subset of countries
rsaeff = addEffPlot(fit = effdatSub, addEffData = effdatSub, row = T) +
  coord_flip() +
  ylab('') + xlab('') +
  theme(
    axis.text.x=element_text(angle=0, size=12),
    axis.text.y=element_text(size=12)
    )
ggsave(rsaeff, file=paste0(pth, 'figure7b.pdf'), height=6, width=4)
############################################
