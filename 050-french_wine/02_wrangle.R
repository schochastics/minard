library(tidyverse)
library(igraph)
library(edgebundle)
library(sf)

source("0001-helper/helper.R")
centroids <- get_centroids()
dummy_ocean <- nodes_ocean()
dummy_land <- nodes_land()

#-------------------------------------------------------------------------------#
setwd("050-french_wine/")

#-------------------------------------------------------------------------------#

wine <- read_csv("data/wine_exports00_18.csv") %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(iso_a3=str_to_upper(iso_3)) %>% 
  select(-iso_3)

wine$iso_a3[wine$country=="Belgium-Luxembourg"] <- "BEL"
wine <- wine %>% arrange(-trade_value) %>% 
  left_join(centroids,by=c("iso_a3")) %>% 
  dplyr::filter(!is.na(x))

wine <- wine %>% 
  group_by(country,continent,iso_a3,x,y) %>%
  dplyr::summarise(trade_value=sum(trade_value),.groups="drop") %>% 
  arrange(-trade_value) %>% 
  top_n(50,trade_value)
  
# graph 
verts <- wine %>% select(country,continent,iso_a3,x,y)
verts <- bind_rows(tibble(country="France",continent="Europe",iso_a3="FRA",x=centroids$x[centroids$iso_a3=="FRA"],y=centroids$y[centroids$iso_a3=="FRA"]),verts)
edges <- wine %>% mutate(from="France") %>% select(from,country,trade_value) %>% rename(to=country,weight=trade_value)
g <- graph_from_data_frame(edges,TRUE,verts)
root <- which(V(g)$name=="France")
xy <- cbind(V(g)$x,V(g)$y)

gtree <- tnss_tree(g,xy,dummy_ocean,root,gamma = 0.85,epsilon = 0.7,elen = 9.5,order = "far")
which(degree(gtree)==0)
plot(gtree,vertex.size=0.5,vertex.label=NA)

if(any(degree(gtree)==0)){
  g1 <- induced_subgraph(g,c("France",names(which(degree(gtree)==0))))
  root <- which(V(g1)$name=="France")
  xy <- cbind(V(g1)$x,V(g1)$y)
  gtree1 <- tnss_tree(g1,xy,dummy_land,root,gamma = 0.85,epsilon = 0.5,elen = 6.5,order = "near")
  gtree <- netUtils::delete_isolates(gtree)
}
id <- str_pad(max(parse_number(list.files("results",pattern = "gtree",full.names = TRUE)))+1,width = 3,side = "left",pad = "0")
V(gtree)$continent <- V(g)$continent[match(V(gtree)$name,V(g)$name)]
V(gtree1)$continent <- V(g)$continent[match(V(gtree1)$name,V(g)$name)]
saveRDS(list(ocean=gtree,land=gtree1),paste0("results/gtree_",id,".RDS"))




saveRDS(gtree,"results/gtree_013.RDS")


# wine imports in GBR
# sitc=1121 Wine
# id3_char="gbr" United Kingdom
# xsv search -s dest 'gbr' year_origin_destination_sitc_rev2.csv > gbr_import.csvxsv search -s dest 'gbr' year_origin_destination_sitc_rev2.csv > gbr_import.csv
# xsv search -s sitc4 '1121' gbr_import.csv > gbr_wine_import.csv

# xsv search -s origin 'fra' year_origin_destination_sitc_rev2.csv > fra_export.csv

#xsv search -s sitc4 '1121' year_origin_destination_sitc_rev2.csv > wine.csv