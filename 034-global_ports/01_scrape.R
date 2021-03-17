library(tidyverse)
library(rvest)

# url <- "https://www.worldshipping.org/about-the-industry/global-trade/top-50-world-container-ports"
url <- "https://en.wikipedia.org/wiki/List_of_busiest_container_ports"
doc <- read_html(url)
tbl <- as_tibble(html_table(doc)[[1]]) %>% 
  janitor::clean_names() %>% 
  select(2:6) %>% 
  rename(traffic=5) %>% 
  mutate(traffic=parse_number(traffic))

tbl$link <- doc %>% 
  html_nodes("td:nth-child(2)") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  .[1:52] %>% 
  .[-46] %>% 
  paste0("https://en.wikipedia.org/",.)

tbl$loc <- ""
for(i in 1:nrow(tbl)){
  cat(i,"\r")
  tbl$loc[i] <- tbl$link[i] %>% 
    read_html() %>% 
    html_nodes(".adr+ tr td") %>% 
    html_text() %>% 
    word(-2,sep="/") %>% 
    str_trim()
}

for(i in 1:nrow(tbl)){
  cat(i,"\r")
  if(is.na(tbl$loc[i])){
    tbl$loc[i] <- paste0("https://en.wikipedia.org/wiki/",str_replace_all(tbl$port[i]," ","_")) %>% 
      read_html() %>% 
      html_node(".geo-dec") %>% 
      html_text() 
  }
}

tbl$loc[23] <- "40.712740°N 74.005974°W"
tbl$loc[37] <- "32.128705°N 81.151907°W"
tbl$loc[42] <- "9.35722°N 79.89861°W"


world <- map(wo)
tbl <- tbl %>% separate(loc,into=c("lat","lon"),sep=" ") %>% 
  mutate(lat=ifelse(str_detect(lat,"S"),paste0("-",lat),lat)) %>% 
  mutate(lon=ifelse(str_detect(lon,"W"),paste0("-",lon),lon)) %>% 
  mutate(lat=parse_number(lat),lon=parse_number(lon))

write_csv(tbl,"data/ports_wiki.csv")

