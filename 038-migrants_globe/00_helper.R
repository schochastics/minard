clean_raw <- function(){
  dat <- readxl::read_xlsx("data/Abel-Database-s2.xlsx",sheet = 4) #200510
  
  dict <- select(dat,2,3) %>% 
    rename(country=1,iso_a3=2) %>% 
    dplyr::filter(!is.na(country))
  write_csv(dict,"data/dict.csv")
  
  dat[-2,] %>% .[,-c(1,3)]
  mat <- as.matrix(dat[3:199,4:ncol(dat)])
  mode(mat) <- "numeric"
  rownames(mat) <- colnames(mat) <- NULL
  mat <- mat[-nrow(mat),]
  mat <- mat[,-ncol(mat)]
  
  migration <- expand_grid(from=dict$country,to=dict$country)
  migration$count <- c(t(mat))
  
  migration <- migration %>% dplyr::filter(count!=0)
  write_csv(migration,"data/migration2005_2010.csv")
}



make_dummies <- function(frac=0.0015){
  real <- vroom::vroom("../000-data/ship_routes.csv")
  xy_dummy <- real %>% sample_frac(frac) %>% select(lon,lat) %>% as.matrix()
  
  # South Africa to Australia
  tseq <- seq(0,1,length.out = 500)
  xy_dummy <- rbind(xy_dummy,
                    matrix(
                      tseq*c(centroids$x[centroids$iso_a3=="AUS"],centroids$y[centroids$iso_a3=="AUS"]) + 
                        (1-tseq)*c(centroids$x[centroids$iso_a3=="ZAF"],centroids$y[centroids$iso_a3=="ZAF"]),
                      ncol=2,byrow = TRUE
                    ))
  
  #to South Sudan
  toSudan <- matrix(
    tseq*c(centroids$x[centroids$iso_a3=="SSD"],centroids$y[centroids$iso_a3=="SSD"]) + 
      (1-tseq)*c(17.270634,35.339740),   
    ncol=2,byrow = TRUE
  )
  
  toSudan <- cbind(jitter(toSudan[,1],factor=50),jitter(toSudan[,2],factor = 50))
  xy_dummy <- rbind(xy_dummy,toSudan)
  
  #to Burundi
  toBurundi <- matrix(
    tseq*c(centroids$x[centroids$iso_a3=="BDI"],centroids$y[centroids$iso_a3=="BDI"]) + 
    (1-tseq)*c(9.045940,-4.477033),   
    ncol=2,byrow = TRUE
  )
  toBurundi <- cbind(jitter(toBurundi[,1],factor=70),jitter(toBurundi[,2],factor = 70))
  xy_dummy <- rbind(xy_dummy,toBurundi)
  
  xy_dummy <- xy_dummy[!(xy_dummy[,1]>=30 & xy_dummy[,1]<=50 &xy_dummy[,2]<=30 &xy_dummy[,2]>=10 ),]
  # xy_dummy <- rbind(xy_dummy,cbind(runif(400,-110,-70),runif(400,-50,0)))
  
  xy_dummy[!duplicated(xy_dummy),]
}

get_graph <- function(migration,root_ctry,centroids){
  migration %>% 
    dplyr::filter(from=="United Kingdom") %>% 
    arrange(-count) %>% 
    top_n(5,count)
  verts <- centroids %>% select(country=name,x,y) %>% semi_join(migration,by=c("country"="to"))
  verts <- bind_rows(tibble(country=root_ctry,
                            x=centroids$x[centroids$name==root_ctry],
                            y=centroids$y[centroids$name==root_ctry]),verts)
  
  edges <- migration %>% select(from,to,count) %>% rename(weight=count)
  g <- graph_from_data_frame(edges,TRUE,verts)
  g
}