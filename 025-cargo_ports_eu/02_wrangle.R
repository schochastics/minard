library(tidyverse)

dat <- read_tsv("../000-data/eu_ports/mar_go_qmc.tsv") %>% janitor::clean_names()
dat <- dat %>% separate(unit_cargo_direct_rep_mar_time,c("unit","cargo","direct","ident"),sep=",")
dict <- vroom::vroom("../000-data/eu_ports/dict.tsv",col_names = FALSE) %>% rename(ident=1,city=2) %>% mutate(ident=str_remove_all(ident,"\\[|\\]"))

tst <- dat %>% 
  dplyr::filter(direct=="TOTAL") %>% select(c(1:4,6)) %>% 
  dplyr::filter(nchar(ident)>4) %>% 
  dplyr::filter(x2020q2!=":") %>% 
  left_join(dict,by="ident") %>% 
  dplyr::filter(!is.na(city) & !str_detect(city,"other ports")) %>% 
  mutate(x2020q2=as.numeric(x2020q2))

geoloc <- tst %>% pull(city)
tst2 <- tidygeocoder::geocode(tst,city = city,method="osm")

dict <- left_join(dict,select(tst2,ident,lat,long),by="ident") %>% distinct()
write_csv(dict,"data/dict.csv")

# build 2019 data
dat %>% 
  dplyr::filter(direct=="TOTAL") %>% 
  select(c(1:4,8:11)) %>% 
  dplyr::filter(nchar(ident)>4) %>% 
  dplyr::filter(cargo=="TOTAL") %>% 
  mutate_at(c(5:8),str_replace_all,pattern=":",replacement="0") %>% 
  mutate_at(c(5:8),as.numeric) %>% 
  select(-c(1:3)) %>% 
  pivot_longer(2:5) %>% 
  group_by(ident) %>% 
  dplyr::summarise(total=sum(value),.groups="drop") %>% 
  dplyr::filter(total!=0) %>% 
  left_join(dict,by="ident") %>% 
  dplyr::filter(!is.na(lat)) %>% 
  write_csv("data/cargo2019.csv")
  
