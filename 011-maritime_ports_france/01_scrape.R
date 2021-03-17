library(rvest)
url <- "http://www.worldportsource.com/ports/index/FRA.php"
doc <- read_html(url)

links <- html_nodes(doc,"br+ a") %>% 
  html_attr("href") %>% 
  paste0("http://www.worldportsource.com",.)

name <- html_nodes(doc,"br+ a") %>% 
  html_text()

ports <- tibble(name,link=links) %>% distinct() %>% mutate(link=str_remove(link,"review/"))
ports$lat <- ports$lon <- ports$deglat <- ports$deglon <- ports$port_type <- ports$port_size <- NA
for(i in 1:nrow(ports)){
  cat(i,"\r")
  th1 <- ports$link[i] %>% 
    read_html() %>% 
    html_nodes("th") %>% html_text()
  
  th2 <- ports$link[i] %>% 
    read_html() %>% 
    html_nodes(".dash") %>% html_text() %>% .[seq(2,length(.),2)]
  
  ports$deglat[i] <- th2[which(th1=="Latitude:")]
  ports$deglon[i] <- th2[which(th1=="Longitude:")]
  latlong <- OSMscale::degree(ports$deglat[i],ports$deglon[i],drop = TRUE)
  ports$lat[i] <- latlong[1]
  ports$lon[i] <- latlong[2]
  ports$port_type[i] <- th2[which(th1=="Port Type:")]
  ports$port_size[i] <- th2[which(th1=="Port Size:")]
}

write_csv(ports,"data/ports_france.csv")
