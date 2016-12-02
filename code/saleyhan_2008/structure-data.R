########
# produce Amen data
#######

#
rm(list=ls())


# load libraries
packs = c('dplyr', 'ggplot2', 'ggthemes', 'readr', 'lmtest', 
          'igraph', 'countrycode')

#
source("LoadPkg.R")

#
loadPkg(packs)

# read data and add variables
sal = 
  read_tsv('/Users/juanftellez/Dropbox/netsMatter/replications/saleyhan2008/input data/RefugeesWar_directed.tab') %>% 
  filter(., year >= 1955) %>% 
  mutate(., mccapshare = capshare - .5) %>% 
  mutate(., capref1 = mccapshare*logref1) %>% 
  mutate(., capref2 = mccapshare*logref2)


# base variables
base_vars_mod1 = c('mzinit_lead logref1 logref2 uppcivcon1 uppcivcon2 dem1 dem2 demdem trans1 trans2 transtrans contig colcont capshare s_wt_glo depend1 depend2 igos lpcyrs lpcyrs1 lpcyrs2 lpcyrs3')
base_vars_mod2 = c('mzinit_lead logref1 logref2 capref1 capref2 uppcivcon1 uppcivcon2 dem1 dem2 demdem trans1 trans2 transtrans contig colcont capshare s_wt_glo depend1 depend2 igos lpcyrs lpcyrs1 lpcyrs2 lpcyrs3')


base_vars_mod1 = unlist(strsplit(base_vars_mod1, split = " "))
base_vars_mod2 = unlist(strsplit(base_vars_mod2, split = " "))


# get rid of missingness
mod1_dat = 
  select(sal, one_of(base_vars_mod1), dyad, year, ccode1, ccode2) %>% 
  filter(complete.cases(.)) %>% 
  mutate(cname1 = countrycode(sourcevar = ccode1, 
                              origin = 'cown', destination = 'country.name')) %>% 
  mutate(cname2 = countrycode(sourcevar = ccode2, 
                              origin = 'cown', 
                              destination = 'country.name'))

mod2_dat = 
  select(sal, one_of(base_vars_mod2), dyad, year, ccode1, ccode2) %>% 
  filter(complete.cases(.)) %>% 
  mutate(cname1 = countrycode(sourcevar = ccode1, 
                              origin = 'cown', destination = 'country.name')) %>% 
  mutate(cname2 = countrycode(sourcevar = ccode2, 
                              origin = 'cown', 
                              destination = 'country.name'))


# Y
Y = list()

for(ii in sort(unique(mod1_dat$year)))
{

  # isolate yearly edgelist
  temp_dat = filter(mod1_dat, year == ii) %>% 
    select(., cname1, cname2, mzinit_lead)
  
  # convert to adjacency matrix
  temp_graph = graph.data.frame(temp_dat, directed = T)
  temp_adj = get.adjacency(temp_graph, attr = 'mzinit_lead', sparse = F)
  
  # add to list
  Y[[paste(ii)]] = temp_adj
  
  #clean up
  rm(temp_graph, temp_adj, temp_dat)

}


# dyadic covariates (xDyadL) [n x n x p ; p = dyadic variable]
# notes: missing time variables
dyad_vars = c('logref1', 'logref2', 'contig', 'colcont', 'capshare', 'demdem', 
              'transtrans', 's_wt_glo', 'igos')

dyad_vars_dir = c('logref1', 'logref2', 'capshare')

Xdyad = list()


for(ii in sort(unique(mod1_dat$year)))
{
  
  # format array
  dim_num = max(c(length(unique(mod1_dat$cname1[mod1_dat$year == ii])), length(unique(mod1_dat$cname2[mod1_dat$year == ii]))))
  
  
  
  temp_array = array(data = NA, dim = c(dim_num,dim_num,length(dyad_vars)))
  
  
  for(jj in 1:length(dyad_vars))
  {
    ###### directed varaibles
    
    if(dyad_vars[jj] %in% dyad_vars_dir) {
      
      # isolate yearly edgelist
      temp_dat = filter(mod1_dat, year == ii) %>% 
        select(., cname1, cname2, one_of(dyad_vars[jj]))
      
      # convert to adjacency matrix
      temp_graph = graph.data.frame(temp_dat, directed = T)
      temp_adj = get.adjacency(temp_graph, attr = dyad_vars[jj], sparse = F)
      
      # fill array
      temp_array[ , ,jj] = temp_adj
      print(jj)
      
    } else {
   
    ###### undirected variables   
      
    # isolate yearly edgelist
    temp_dat = filter(mod1_dat, year == ii) %>% 
      select(., cname1, cname2, one_of(dyad_vars[jj]))
    
    # convert to adjacency matrix
    temp_graph = graph.data.frame(temp_dat, directed = T)
    temp_adj = get.adjacency(temp_graph, attr = dyad_vars[jj], sparse = F)
    
    # fill array
    temp_array[ , ,jj] = temp_adj
    print(jj) }
  }
  
  # add to list
  Xdyad[[paste(ii)]] = temp_array
  
  print(ii)
  
}


# monadic vars
monad_vars_a = c('uppcivcon1', 'dem1', 'trans1', 'lpcyrs', 'lpcyrs1', 
                 'lpcyrs2', 'lpcyrs3')
monad_vars_b = c('uppcivcon2', 'dem2', 'trans2', 'lpcyrs', 'lpcyrs1', 
                 'lpcyrs2', 'lpcyrs3')  

xNode = list()

for(ii in sort(unique(mod1_dat$year)))
{
  # get in monad level
  temp_dat1 = 
    filter(mod1_dat, year == ii) %>% 
    select(., uppcivcon = uppcivcon1, dem = dem1, trans = trans1, 
           cname1) %>% 
    distinct()
  
  temp_dat2 = 
    filter(mod1_dat, year == ii) %>% 
    select(., uppcivcon = uppcivcon2, dem = dem2, trans = trans2, 
           cname1 = cname2) %>% 
    distinct()
  
  # join
  temp_join = rbind(temp_dat1, temp_dat2) %>%  distinct()
  
  # put in list
  xNode[[paste(ii)]] = temp_join
  print(ii)
}


# save objects
save(Y, xDyad, xNode, file = '/Users/juanftellez/Dropbox/netsMatter/replications/saleyhan2008/output data/amenData.rda')

