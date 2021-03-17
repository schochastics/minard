library(tidyverse)
library(igraph)
library(edgebundle)
library(sf)

source("0001-helper/helper.R")
centroids <- get_centroids()
dummy_ocean <- nodes_ocean()
dummy_land <- nodes_land()
setwd("041-english_coal_1997/")
#-------------------------------------------------------------------------------#

coal <- read_csv("data/coal.csv") %>% 
  mutate(export_val=ifelse(export_val=="NULL","0",export_val),
         import_val=ifelse(import_val=="NULL","0",import_val)) %>% 
  mutate_at(c("export_val","import_val"),as.numeric) %>% 
  dplyr::filter(origin=="gbr",dest!="wld",dest!="fdr",year==1997) %>% 
  arrange(-export_val) %>% 
  dplyr::filter(export_val>0) %>% 
  mutate(origin=str_to_upper(origin),dest=str_to_upper(dest)) %>% 
  top_n(25,export_val)
  
# graph 
verts <- centroids %>% select(iso_a3,country=name,x,y) %>% semi_join(coal,by=c("iso_a3"="dest"))
verts <- bind_rows(tibble(iso_a3="GBR",country="United Kingdom",x=centroids$x[centroids$iso_a3=="GBR"],y=centroids$y[centroids$iso_a3=="GBR"]),verts)
edges <- coal %>% select(origin,dest,export_val) %>% rename(weight=export_val)
g <- graph_from_data_frame(edges,TRUE,verts)
V(g)$iso_a3 <- V(g)$name
V(g)$name <- V(g)$country 
root <- which(V(g)$name=="United Kingdom")
xy <- cbind(V(g)$x,V(g)$y)

gtree <- tnss_tree(g,xy,dummy_ocean,root,gamma = 0.85,epsilon = 0.5,elen = 8.5,order = "near")
which(degree(gtree)==0)
plot(gtree,vertex.size=0.5,vertex.label=NA)

if(any(degree(gtree)==0)){
  g1 <- induced_subgraph(g,c("United Kingdom",names(which(degree(gtree)==0))))
  root <- which(V(g1)$name=="United Kingdom")
  xy <- cbind(V(g1)$x,V(g1)$y)
  gtree1 <- tnss_tree(g1,xy,dummy_land,root,gamma = 0.85,epsilon = 0.5,elen = 6.5,order = "near")
  gtree <- netUtils::delete_isolates(gtree)
}
id <- str_pad(max(parse_number(list.files("results",pattern = "gtree",full.names = TRUE)))+1,width = 3,side = "left",pad = "0")

saveRDS(list(ocean=gtree,land=gtree1),paste0("results/gtree_",id,".RDS"))
