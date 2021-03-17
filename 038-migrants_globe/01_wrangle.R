library(tidyverse)
library(igraph)
library(edgebundle)

source("0001-helper/helper.R")
centroids <- get_centroids()
dummy_ocean <- nodes_ocean()
dummy_land <- nodes_land()
setwd("038-migrants_globe/")
source("00_helper.R")


# centroids of countries
dict <- read_csv("data/dict.csv",
                 col_types = cols(
                   country = col_character(),
                   iso_a3 = col_character(),
                   continent = col_character()
                 ))

migration <- read_csv("data/migration2005_2010.csv",
                      col_types = cols(
                        from = col_character(),
                        to = col_character(),
                        count = col_double()
                      ))

# ctry migration ----------------------------------------------------------------#
# countries <- c("United Kingdom","China","India","France","Germany","Republic of Congo")
countries <- c("United Kingdom","China","India","Nigeria","Brazil")

gtree_lst <- vector("list",length(countries))
k <- 1
for(ctry in countries){
  cat(k,"\r")
  conti <- dict$continent[dict$country==ctry]
  migration_ctry <- migration %>% 
    dplyr::filter(from==ctry) %>% 
    left_join(select(dict,-iso_a3),by=c("to"="country")) %>% 
    dplyr::filter(continent!=conti) %>% 
    arrange(-count) %>% 
    top_n(5,count)
  
  g <- get_graph(migration_ctry,ctry,centroids)
  root <- which(V(g)$name==ctry)
  xy <- cbind(V(g)$x,V(g)$y)
  gtree_lst[[k]] <- tnss_tree(g,xy,dummy_ocean,root,gamma = 0.85,epsilon = 0.7,elen = 9.5,order = "near")  
  k <- k+1
}
id <- str_pad(max(parse_number(list.files("results",pattern = "gtree_lst",full.names = TRUE)))+1,width = 3,side = "left",pad = "0")
saveRDS(gtree_lst,paste0("results/gtree_lst_",id,".RDS"))





# all migration with edge bundling----------------------------------------------#
# graph 
verts <- centroids %>% select(country=name,x,y)

edges <- migration %>% 
  select(from,to,count) %>% 
  rename(weight=count) %>% 
  dplyr::filter(weight>=100) %>% 
  dplyr::filter(from%in%verts$country & to%in%verts$country)
g <- graph_from_data_frame(edges,TRUE,verts)
xy <- cbind(V(g)$x,V(g)$y)

hbundle <- edge_bundle_hammer(g,xy)
fbundle <- edge_bundle_force(g,xy)
hbundle$weight <- E(g)$weight[hbundle$group]
hbundle$size <- ambient::normalise(hbundle$weight,to = c(0.05,0.5))

fbundle$weight <- E(g)$weight[fbundle$group]
fbundle$size <- ambient::normalise(fbundle$weight,to = c(0.05,0.5))

saveRDS(list(graph=g,coords=xy,hbundle=hbundle,fbundle=fbundle),"results/bundling_all.RDS")
