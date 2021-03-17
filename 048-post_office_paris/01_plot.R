library(sf)
library(tidyverse)
library(rvest)
library(ggtext)
#https://www.data.gouv.fr/, https://en.wikipedia.org/wiki/Quarters_of_Paris, https://www.naturalearthdata.com/

setwd("048-post_office_paris/")

arrond <- st_read("data/fra_shape/arrondissements.shp")
paris <- st_read("data/fra_shape/quartier_paris.shp")
mail_tbl <- read_csv("data/mail_boxes.csv") %>% 
  slice_sample(n=358) #original data has 358 mailboxes
post_tbl <- read_csv("data/post_offices.csv") %>% 
  slice_sample(n=35) #original data has 35 postofices
# rivers <- st_read("000-data/naturalearth/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")
# seine <- rivers[which(rivers$name=="Seine"),]
# seine_pts <- st_coordinates(st_cast(seine,"POINT"))
# seine_pts <- as_tibble(seine_pts) %>% 
#   dplyr::filter(Y<=48.9,Y>=48.80,X>=2.25,X<=2.42)
set.seed(1234)
cols <- sample(rep(c("#F9D8D6","#CDF5F6","#F9EBDF","#EFD2AC"),5))

tbl <- "https://en.wikipedia.org/wiki/Quarters_of_Paris" %>% 
  read_html() %>% 
  html_table(fill=TRUE) %>% 
  .[[1]] %>% 
  janitor::clean_names() %>% 
  select(-1,-map) %>% 
  rename(number=1,quartier=2,pop=3,area=4) %>% 
  mutate(pop=str_remove(pop,",") %>% as.numeric()) %>% 
  mutate(number=parse_number(number))

paris <- paris %>% left_join(tbl,by=c("c_qu"="number")) %>% arrange(c_qu)
xy <- st_coordinates(st_centroid(paris$geometry))

set.seed(43134)
dat <- tibble(x=xy[,1],y=xy[,2],pop=paris$pop,c_qu=1:80) %>% 
  mutate(pop_scale=ambient::normalise(pop,to=c(4,12))) %>% 
  bind_cols(
    map_dfr(1:nrow(paris),function(x) 
      paris$geometry[[x]] %>% st_sample(1) %>% st_coordinates %>% as_tibble() %>% rename(xlab=X,ylab=Y))
  ) %>% 
  mutate(xlab=0.2*x+0.8*xlab,ylab=0.2*y+0.8*ylab) %>% 
  mutate(ylab=ifelse(c_qu%in%c(6,8,10,69,15),ylab+0.003,ylab)) %>% 
  mutate(xlab=ifelse(c_qu%in%c(3,11,12,14),xlab+0.003,xlab))

map_title <- "<span style='font-size:32pt;'>Figurative Map to determine the location of a new post office in Paris</span>"

descr <-"<span style='font-size:26pt'>Legend </span>  
<span style='font-size:16pt'>
The double parallel lines delineate the limits  
of the 20 arrondissements and the dotted lines  
those of the 80 quarters, which are distinguished  
by the number for each;  
the colors indicate the arrondisments.  
</span> <br>
<span style='font-size:16pt'>
The area of each black square represents the    
population of the quartier. This population  
is also written under the squares.  
</span><br>
<span style='font-size:16pt'>
The small black circles are a sample of 358 mailboxes.  
The doubled circles are the post offices.
</span>
" 

ggplot()+
  geom_sf(data=paris,size=0.5,col="black",aes(fill=factor(c_ar)),linetype=2,show.legend = FALSE)+
  geom_sf(data=arrond,size=1.3,col="black",fill=NA)+
  geom_sf(data=arrond,size=1,col="white",fill=NA)+
  geom_point(data=dat,aes(x,y,size=I(pop_scale)),shape=15,show.legend = FALSE)+
  geom_point(data=mail_tbl,aes(x,y),size=2.5)+
  geom_point(data=post_tbl,aes(x,y),size=3.5,col="black")+
  geom_point(data=post_tbl,aes(x,y),size=3,col="white")+
  geom_point(data=post_tbl,aes(x,y),size=2.5,col="black")+
  geom_text(data=dat,aes(xlab,ylab,label=c_qu),col="black",family="EB Garamond 08",size=6)+
  geom_text(data=dat,aes(x,y-0.00035*pop_scale/2,label=format(pop,big.mark = ",")),col="black",family="EB Garamond 08",size=2)+
  scale_fill_manual(values=cols)+
  coord_sf(ylim=c(48.81,48.905))+
  #title
  annotate("richtext",x=2.35,y=Inf,hjust=0.5,vjust=1,label=map_title,
           size=8,fill="#e8e4c9",family="EB Garamond 08",
           label.padding = unit(c(1.0, 1.0, 1.0, 1.0), "lines"))+
  #descr
  annotate("richtext",x=-Inf,y=-Inf,hjust=-0.1,vjust=0,label=descr,
           fill=NA,family="ChopinScript",color="black",label.colour=NA,
           label.padding = unit(c(0.1, 0.1, 0.1, 0.1), "lines"))+
  #caption
  annotate("richtext",x=Inf,y=-Inf,hjust=1,vjust=0,label="<span style='font-size:6pt'>redrawn by *@schochastics*</span>",
           size=8,fill=NA,family="EB Garamond 08",label.color=NA,
           label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines"))+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#e8e4c9",colour="black",size=0.1),
    panel.background = element_rect(fill="#e8e4c9",colour="black",size=1))

w <- 20
h <- 0.721244 * w
ggsave("048-post_office_paris.png",width = w,height = h,dpi = 300)
