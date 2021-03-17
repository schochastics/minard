nodes_ocean <- function(remove_islands=TRUE){
  land <- sf::st_read("000-data/naturalearth/ne_110m_land/ne_110m_land.shp")
  if(remove_islands){
    land <- rmapshaper::ms_filter_islands(land,min_area = 5e10)
  }
  
  ocean <- sf::st_read("000-data/naturalearth/ne_110m_ocean/ne_110m_ocean.shp")
  
  xy <- poissoned::poisson_disc(180,90,2)
  xy[,1] <- xy[,1]-180
  xy[,2] <- xy[,2]-90
  pts  <- sf::st_as_sf(xy,coords = 1:2,crs = 4326)
  
  log <- apply(st_within(pts,ocean,sparse=F),1,any)
  xy <- xy[log,]
  pts  <- sf::st_as_sf(xy,coords = 1:2,crs = 4326)

  land_buf <- sf::st_buffer(land,2,endCapStyle="FLAT",nQuadSegs=45)
  log <- !apply(sf::st_within(pts,land_buf,sparse=F),1,any)
  xy <- as.matrix(xy[log,])
  colnames(xy) <- NULL
  xy
}

nodes_land <- function(remove_islands=TRUE){
  land <- sf::st_read("000-data/naturalearth/ne_110m_land/ne_110m_land.shp")
  if(remove_islands){
    land <- rmapshaper::ms_filter_islands(land,min_area = 5e10)
  }
  
  xy <- poissoned::poisson_disc(180,90,2)
  xy[,1] <- xy[,1]-180
  xy[,2] <- xy[,2]-90
  pts  <- sf::st_as_sf(xy,coords = 1:2,crs = 4326)
  
  log <- apply(st_within(pts,land,sparse=F),1,any)
  xy <- xy[log,]
  pts  <- sf::st_as_sf(xy,coords = 1:2,crs = 4326)
  # land_buf <- sf::st_buffer(land,2,endCapStyle="FLAT",nQuadSegs=45)
  # log <- !apply(sf::st_within(pts,land_buf,sparse=F),1,any)
  # xy <- xy[log,]
  xy <- as.matrix(xy)
  colnames(xy) <- NULL
  xy
}

graph_union <- function(gtree){
  if(!all(sapply(gtree,function(x) igraph::is.igraph(x)))){
    stop("glist must be a list of igraph objects")
  }
  vattrs <- unique(c(sapply(gtree,igraph::vertex_attr_names)))
  dflist <- lapply(gtree,igraph::as_data_frame,what="both")
  verts <- lapply(dflist,function(x) x$vertices)
  edges <- lapply(dflist,function(x) x$edges)
  
  dummy_ids <- names(verts)
  
  verts <- lapply(1:length(dummy_ids),function(x){
    df <- verts[[x]]
    df$name <- str_replace_all(df$name,"dummy",paste0(dummy_ids[x],"_dummy"))
    df
  })
  
  edges <- lapply(1:length(dummy_ids),function(x){
    df <- edges[[x]]
    df$from <- str_replace_all(df$from,"dummy",paste0(dummy_ids[x],"_dummy"))
    df$to <- str_replace_all(df$to,"dummy",paste0(dummy_ids[x],"_dummy"))
    df
  })
  verts <- do.call("bind_rows",verts)
  verts <- verts[!duplicated(verts),]
  rownames(verts) <- NULL
  edges <- do.call("bind_rows",edges)
  rownames(edges) <- NULL
  # verts <- verts[,c(4,1,2,3)]
  verts <- verts[,c(which(vattrs=="name"),which(vattrs!="name"))]
  graph_from_data_frame(edges,FALSE,verts)  
}

get_centroids <- function(){
  centroids <- read_csv("000-data/country_centroids_az8.csv")
  centroids$name_long[centroids$name_long == "Republic of Korea"] <- "South Korea"
  centroids$name_long[centroids$name_long == "Dem. Rep. Korea"] <- "North Korea"
  
  centroids <- centroids %>% select(name_long,iso_a3,the_geom)
  centroids <- centroids %>% 
    rename(name = name_long) %>% 
    mutate(the_geom=str_remove_all(the_geom,"POINT \\(|\\)")) %>% 
    separate(the_geom,c("x","y"),sep = " ",convert = TRUE)
  
  # fix some countries
  centroids$y[centroids$iso_a3=="USA"] <- 40.730610
  centroids$x[centroids$iso_a3=="USA"] <- -73.935242
  
  centroids$y[centroids$iso_a3=="CAN"] <-45.508888
  centroids$x[centroids$iso_a3=="CAN"] <--73.561668
  
  centroids$y[centroids$iso_a3=="FRA"] <-47.020468
  centroids$x[centroids$iso_a3=="FRA"] <--2.030925
  
  centroids$y[centroids$iso_a3=="BRA"] <--22.742982
  centroids$x[centroids$iso_a3=="BRA"] <--43.219908
  
  centroids$y[centroids$iso_a3=="MEX"] <- 22.260472
  centroids$x[centroids$iso_a3=="MEX"] <--97.762193
  
  centroids$y[centroids$iso_a3=="CHN"] <- 24.716764
  centroids$x[centroids$iso_a3=="CHN"] <-118.675790
  
  centroids$y[centroids$iso_a3=="RUS"] <- 59.957107
  centroids$x[centroids$iso_a3=="RUS"] <- 30.166003
  
  centroids$y[centroids$iso_a3=="AUS"] <- -37.858561
  centroids$x[centroids$iso_a3=="AUS"] <- 144.871746
  
  centroids$y[centroids$iso_a3=="ZAF"] <--34.819684 #-34.045057, 18.479713
  centroids$x[centroids$iso_a3=="ZAF"] <-19.909520
  
  centroids$y[centroids$iso_a3=="NGA"] <-4.546053 
  centroids$x[centroids$iso_a3=="NGA"] <-5.782492
  
  centroids$y[centroids$iso_a3=="DEU"] <-53.557749 
  centroids$x[centroids$iso_a3=="DEU"] <-8.412779
  
  centroids$y[centroids$iso_a3=="SWE"] <-57.577221
  centroids$x[centroids$iso_a3=="SWE"] <-11.970901
  
  centroids$y[centroids$iso_a3=="NOR"] <-58.144448
  centroids$x[centroids$iso_a3=="NOR"] <-7.899059
  
  centroids$y[centroids$iso_a3=="FIN"] <-60.167458
  centroids$x[centroids$iso_a3=="FIN"] <-24.608889
  
  centroids$y[centroids$iso_a3=="IND"] <-13.152419
  centroids$x[centroids$iso_a3=="IND"] <-74.847109
  
  centroids$y[centroids$iso_a3=="PAK"] <-24.853159
  centroids$x[centroids$iso_a3=="PAK"] <-66.734433
  
  centroids$y[centroids$iso_a3=="ISL"] <-63.780984
  centroids$x[centroids$iso_a3=="ISL"] <--17.537521
  
  centroids$y[centroids$iso_a3=="CHL"] <--27.807285
  centroids$x[centroids$iso_a3=="CHL"] <--71.017134
  
  centroids$y[centroids$iso_a3=="TUR"] <-36.845118
  centroids$x[centroids$iso_a3=="TUR"] <-31.180735
  
  centroids$y[centroids$iso_a3=="ARG"] <--38.560257
  centroids$x[centroids$iso_a3=="ARG"] <--58.552992
  
  
  centroids <- bind_rows(centroids,tibble(name="Channel Islands",iso_a3="XXX",x=2.317,y=49.433))
  centroids
}
