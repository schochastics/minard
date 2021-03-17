library(tidyverse)
library(sf)

setwd("011-maritime_ports_france/")

rivers <- st_read("../000-data/naturalearth/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")
fra_wa <- rivers[which(rivers$name%in%c("Garonne","Loire","Charente","Seine","Meuse","Dordogne")),][1:4,]
land <- st_read("../000-data/naturalearth/ne_10m_land/ne_10m_land.shp")

ports <- read_csv("data/ports_france.csv")
ports <- ports %>%
  mutate(lon=ifelse(str_detect(deglon,"W"),-lon,lon)) %>% 
  mutate(num_size = case_when(port_size=="Very Small"~0.25,
                              port_size=="Small"~0.5,
                              port_size=="Medium"~1,
                              port_size=="Large"~2
                              )) %>% 
  mutate(radius=num_size/20)

newloc <- packcircles::circleRepelLayout(select(ports,lon,lat,radius))
ports1 <- bind_cols(ports,newloc$layout[,-3])

p <- ggplot()+
  geom_sf(data=land,fill="white",colour="black",size=0.1)+
  geom_sf(data=fra_wa,col="black",size=0.1)+
  geom_sf_text(data=fra_wa[1:4,],aes(label=name),family="EB Garamond 08",size=3,nudge_y = -0.1,nudge_x = 0.1)+
  geom_point(data=ports1,aes(x,y,size=I(num_size*10),fill=name),
             shape=21,stroke=0.15,alpha=0.5,show.legend = TRUE)+
  geom_text(data=ports1,aes(x,y),label=1:nrow(ports1),vjust=-ports1$num_size*6, family="EB Garamond 08",size=1.2)+
  scale_fill_manual(name = "",values = rep("grey86",nrow(ports1)),labels=paste0(1:nrow(ports1)," ",str_remove(ports1$name,"Port of |de ")))+
  coord_sf(xlim=c(-5,6.9),ylim=c(42.5,51.5))+
  theme_void()+
  theme(legend.position = c(0.8,0.5),legend.text = element_text(family="Joscelyn",size=3))+
  guides(fill = guide_legend(ncol = 3,override.aes = list(size=0,alpha=0),keyheight=0.1)) 

h <- 14
w <- h*0.865753
p
ggsave("011-maritime_ports_france.png",p,width = w,height = h,dpi = 300)