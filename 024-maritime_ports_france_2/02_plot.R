library(tidyverse)
library(sf)
library(scatterpie)
library(ggtext)
setwd("024-maritime_ports_france_2/")

land <- st_read("../000-data/naturalearth/ne_10m_land/ne_10m_land.shp")

set.seed(123)
ports <- read_csv("data/ports_france.csv")
ports <- ports %>%
  mutate(lon=ifelse(str_detect(deglon,"W"),-lon,lon)) %>% 
  mutate(num_size = case_when(port_size=="Very Small"~0.25,
                              port_size=="Small"~0.5,
                              port_size=="Medium"~1,
                              port_size=="Large"~2
                              )) %>%
  mutate(rnd=runif(nrow(ports),0.49,0.51)) %>% 
  dplyr::filter(num_size>=0.5 & rnd>0.5) %>% 
  mutate(radius=num_size/20)

newloc <- packcircles::circleRepelLayout(select(ports,lon,lat,radius))
ports1 <- bind_cols(ports,newloc$layout[,-3])

ports1 <- ports1 %>% 
  rowwise() %>% 
  mutate(foreign  = runif(1)) %>% 
  mutate(domestic = 1-foreign) %>% 
  ungroup()
map_title <- "<span style='font-size:36pt'>Figurative Map </span><span style='font-size:28pt'>of the importance <br>of maritime ports of France</span>"
p <- ggplot()+
  geom_sf(data=land,fill="#e8e4c9",colour="black",size=0.1)+
  geom_point(data=ports1,aes(x,y,size=I(num_size*10),col=name),
             shape=21,stroke=0.15,alpha=0.0,show.legend = TRUE)+
  geom_scatterpie(data=ports1,aes(x,y,r=num_size/10),cols = c("foreign","domestic"),size=0.1)+
  geom_text(data=ports1,aes(x,y),label=1:nrow(ports1),vjust=-ports1$num_size*6, family="EB Garamond 08",size=1.2)+
  scale_color_manual(name = "",values = rep("grey86",nrow(ports1)),labels=paste0(1:nrow(ports1)," ",str_remove(ports1$name,"Port of |de ")))+
  scale_fill_manual(values=c("foreign"="#56B1F7","domestic"="#132B43"),guide=FALSE)+
  coord_sf(xlim=c(-5,6.9),ylim=c(42.5,51.5))+
  #caption
  annotate("richtext",x=Inf,y=-Inf,hjust=1,vjust=0,label="<span style='font-size:6pt'>redrawn by *@schochastics*</span>",
           size=8,fill=NA,family="EB Garamond 08",label.color=NA,
           label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines"))+
  #title
  annotate("richtext",x=Inf,y=Inf,hjust=1,vjust=1,label=map_title,
           size=8,fill="#f2eecb",family="EB Garamond 08",
           label.padding = unit(c(3.25, 1.0, 3.0, 1.0), "lines"))+
  theme_void()+
  theme(legend.position = c(0.1,0.2),
        legend.text = element_text(family="ChopinScript",size=6),
        legend.background = element_rect(fill="#f2eecb",colour="black",size=0.5),
        plot.background = element_rect(fill="#f2eecb",colour="black",size=0.5),
        panel.background = element_rect(fill="#f2eecb",colour="black",size=0.1))+
  guides(col = guide_legend(ncol = 2,override.aes = list(size=0,alpha=0),keyheight=0.1)) 

h <- 14
w <- h*0.865753
ggsave("024-maritime_ports_france_2.png",p,width = w,height = h,dpi = 300)

