library(tidyverse)
library(ggtext)
library(sf)
library(scatterpie)

setwd("021-meat_to_paris/")
france <- st_read("../000-data/fra_shape/FRA_adm2.shp")

france <- rmapshaper::ms_simplify(france,keep = 0.001)
france$sent <- "no"
france$sent[c(3:6,8,9,10,64,65,12:19,79:82,71:75,27:30,20:22,47,48,31:36,54:56,23:26,43,57:60,37:40,76:78,69,70,90,91)] <- "yes"
france <- france %>% dplyr::filter(NAME_1!="Corse")

set.seed(1111)
france <- france %>% 
  rowwise() %>% 
  mutate(weight=ifelse(sent=="yes",rexp(1,rate=0.5),NA)) %>% 
  mutate(beef = weight*runif(1,0.4,1)) %>% 
  mutate(veal = (weight-beef)*runif(1)) %>% 
  mutate(lamb = weight-beef-veal) %>% 
  ungroup()

xy <- st_coordinates(st_centroid(france$geometry))
meat_tbl <- tibble(x=xy[,1],y=xy[,2],weight=france$weight,beef=france$beef,veal=france$veal,lamb=france$lamb) %>% 
  dplyr::filter(!is.na(weight))

map_title <- "<span style='font-size:36pt'>Figurative Map </span><span style='font-size:28pt'>of the quatities of red meat <br>sent by the departments to Paris</span>"
descr <-"<span style='font-size:26pt'>Explanatory note </span>  
<span style='font-size:16pt'>
The area of a circle represents the total weight  
of meat supplied by that department; the area of  
the colored sections indicates variety.  
Areas in black represent beef, <span style='color:red'>red</span> indicates veal and  
<span style='color:forestgreen'>green</span> is lamb.
</span>" 
ggplot(france)+
  geom_sf(aes(fill=sent),size=0.3,show.legend = FALSE)+
  geom_sf_text(aes(label=NAME_2),family="ChopinScript",nudge_y = ifelse(france$sent=="no",0,0.1)) +
  geom_scatterpie(data=meat_tbl,aes(x,y,r=sqrt(weight/100)),cols=c("beef","veal","lamb"))+
  scale_fill_manual(values=c("yes"="#EFECB0","no"="#DCCEB3","beef"="black","veal"="firebrick3","lamb"="forestgreen"),guide=FALSE)+
  #title
  annotate("richtext",x=-Inf,y=Inf,hjust=0,vjust=1,label=map_title,
           size=8,fill="#f2eecb",family="EB Garamond 08",
           label.padding = unit(c(3.25, 1.0, 3.0, 1.0), "lines"))+
  #descr
  annotate("richtext",x=-Inf,y=46,hjust=0,vjust=1,label=descr,
           fill="#f2eecb",family="EB Garamond 08",
           label.padding = unit(c(3.25, 1.0, 3.0, 1.0), "lines"))+
  #caption
  annotate("richtext",x=Inf,y=-Inf,hjust=1,vjust=0,label="<span style='font-size:6pt'>redrawn by *@schochastics*</span>",
           size=8,fill=NA,family="EB Garamond 08",label.color=NA,
           label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines"))+
  theme_void()+
  theme(plot.background = element_rect(fill="#f2eecb",colour="black",size=0.5),
        panel.background = element_rect(fill="#f2eecb",colour="black",size=0.1))

h <- 20
w <- h*0.82

ggsave("021-meat_to_paris.png",width=w,height=h,dpi = 300)
