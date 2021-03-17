# metro <- metro_multicriteria(gtree,cbind(V(gtree)$x,V(gtree)$y),l = 3,gr = 1,bsize = 5,w=c(100,1,1,1,500))
# metro <- cbind(V(gtree)$x,V(gtree)$y)
# plot(metro)
# points(cbind(V(gtree)$x,V(gtree)$y),col="red")
# 
# coords <- metro
# coords[V(gtree)$tnss!="dummy",1] <- V(gtree)$x[V(gtree)$tnss!="dummy"]
# coords[V(gtree)$tnss!="dummy",2] <- V(gtree)$y[V(gtree)$tnss!="dummy"]
# 
# idx <- which(unlist(lapply(neighborhood(gtree,3,V(gtree),"all",1),function(l) any(V(gtree)$tnss[l]!="dummy"))))
# coords[idx,1] <- V(gtree)$x[idx]
# coords[idx,2] <- V(gtree)$y[idx]

onland <- rep(FALSE,nrow(xy_dummy))
for(i in 1:nrow(xy_dummy)){
  pts <- st_point(xy_dummy[i,])
  if(any(st_within(pts,world$geometry,sparse=FALSE))){
    onland[i] <- TRUE
  }
}
table(onland)
xy_bkp <- xy_dummy
xy_dummy <- xy_dummy[!onland,]



rect <- st_polygon(list(rbind(c(-150,-50),c(200,-50),c(200,75),c(-150,75),c(-150,-50))))
ocean <- rect
for(i in 100:length(world$geometry)){
  cat(i,"\r")
  ocean <- st_difference(ocean,world$geometry[[i]])
}
saveRDS(ocean,"ocean.RDS")
st_sample(ocean,5)

coast <- ne_countries(scale = "small", type="countries",returnclass = "sf")
ggplot(coast)+geom_sf(fill="red")


xy_dummy <- do.call("rbind",lapply(1:length(pts),function(i) cbind(pts[[i]][["lon"]],pts[[i]][["lat"]])))


medit <- read_sf("mediterraneansea_q5m.shp")
ocean1 <- st_difference(ocean,medit$geometry)

dput(unique(V(g)$continent))
verts1 <- as_data_frame(gtree,"vertices")
sp <- cbind(verts1$x[unlist(shortest_paths(gtree,root,which(V(gtree)$name=="United States"))$vpath[[1]])],
            verts1$y[unlist(shortest_paths(gtree,root,which(V(gtree)$name=="United States"))$vpath[[1]])])
sp <- st_sfc(st_linestring(sp),crs = sf::st_crs(world)$input)

world1 <- world
world1$geometry <- world1$geometry * 0.6
world1$geometry[[which(world$iso_a3=="FRA")]] <- world1$geometry[[which(world$iso_a3=="FRA")]]
ggplot()+
  geom_sf(data=world,fill="#E1E1E1",colour="black",size=0.1)+
  geom_sf(data=sp)+
  theme_graph()+
  coord_sf()

xdiff <- seq(min(xy[,1]),max(xy[,1]),length.out = 75)
ydiff <- seq(min(xy[,2]),max(xy[,2]),length.out = 75)
xy_grid <- as.matrix(expand.grid(xdiff,ydiff))
pts <- st_point(xy_grid)
tmp <- st_within(st_point(xy_grid[2,]),ocean,sparse=FALSE)

africa <- ne_countries(continent = 'africa',returnclass = "sf")
ggplot(st_buffer(africa,dist=0.005))+geom_sf()

tst <- edgebundle::metro_multicriteria(gtree,cbind(V(gtree)$x,V(gtree)$y),l = 1,gr = 0.5,bsize = 5)
plot(tst)
points(cbind(V(gtree)$x,V(gtree)$y),col="red")
coords <- tst
coords[V(gtree)$tnss!="dummy",1] <- V(gtree)$x[V(gtree)$tnss!="dummy"]
coords[V(gtree)$tnss!="dummy",2] <- V(gtree)$y[V(gtree)$tnss!="dummy"]

idx <- which(unlist(lapply(neighborhood(gtree,3,V(gtree),"all",1),function(l) any(V(gtree)$tnss[l]!="dummy"))))
coords[idx,1] <- V(gtree)$x[idx]
coords[idx,2] <- V(gtree)$y[idx]


ggraph(gtree,"manual",x=coords[,1],y=coords[,2])+
  geom_sf(data=world,fill="#E1E1E1",colour=NA,size=0.1)+
  geom_sf(data=france,fill="#E1E1E1",colour="black",size=0.1)+
  geom_sf(data=coast,colour="black",size=0.1)+
  geom_edge_link(aes(width=flow),edge_color="#DCB5A7",lineend = "round",show.legend = FALSE)+
  scale_edge_width(range=c(1,4))+#,trans="sqrt")+
  scale_edge_color_gradient(low="#cc0000",high = "#0000cc")+
  theme_graph()+
  coord_sf(xlim = range(V(gtree)$x),ylim = range(V(gtree)$y))


