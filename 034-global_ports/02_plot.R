library(tidyverse)
library(ggtext)

center_NA <- c(-100.167,45.167)
center_SA <- c(-56.1004,-15.6006)
center_AF <- c(14.100526,1.616742)
center_AS <- c(76.08583,38.48333)
center_AU <- c(132.133,-25.117)

land <- st_read("../000-data/naturalearth/ne_110m_land/ne_110m_land.shp")
land_clip <- sf::st_crop(land,c(xmin=-127.3,ymin=-57.8,xmax=155.7,ymax=66.8))
ports <- read_csv("data/ports_wiki.csv")
ports$size <- ambient::normalize(ports$traffic,to = c(4,8.5)) 
ports$radius <- ports$size

newloc <- packcircles::circleRepelLayout(select(ports,lon,lat,radius))
ports <- bind_cols(ports,newloc$layout[,-3])

map_title <- "<span style='font-size:24pt'>FIGURATIVE MAP of Great Ports of the Globe</span> <br> <span style='font-size:18pt'>Top 50 based on Container Traffic (10M EUs) in 2018</span>"
ggplot()+
  geom_sf(data=land_clip,fill="#e8e4c9",colour="black",size=0.1)+
  geom_point(data=ports,aes(x,y,size=I(size)), 
             shape=21,fill="#ff726f",stroke=0.15,show.legend = FALSE)+
  geom_text(data=ports,aes(x,y,label=round(traffic/1000,1)),
            family="EB Garamond 08",size=1.2)+
  geom_text(data=ports,aes(x,y,label=port),vjust=-ports$size/2,
            family="EB Garamond 08",size=1.2)+
  annotate("richtext",x=center_NA[1],y=center_NA[2],label="*NORTH AMERICA*",size=7,
           fill=NA,colour="black",label.colour=NA,
           lineheight=0.6,vjust=1,family="EB Garamond 08")+
  annotate("richtext",x=center_SA[1],y=center_SA[2],label="*SOUTH AMERICA*",size=7,
           fill=NA,colour="black",label.colour=NA,
           lineheight=0.6,vjust=0,family="EB Garamond 08")+
  annotate("richtext",x=center_AF[1],y=center_AF[2],label="*AFRICA*",size=7,hjust=0,
           fill=NA,colour="black",label.colour=NA,
           lineheight=0.6,vjust=0,family="EB Garamond 08")+
  annotate("richtext",x=center_AS[1],y=center_AS[2],label="*ASIA*",size=7,hjust=0,
           fill=NA,colour="black",label.colour=NA,
           lineheight=0.6,vjust=0,family="EB Garamond 08")+
  annotate("richtext",x=center_AU[1],y=center_AU[2],label="*AUSTRALIA*",size=7,hjust=0.5,
           fill=NA,colour="black",label.colour=NA,
           lineheight=0.6,vjust=0.5,family="EB Garamond 08")+
  coord_sf(expand=FALSE)+
  
  #title
  annotate("richtext",x=-Inf,y=Inf,hjust=0,vjust=1,label=map_title,
           size=8,fill="#f2eecb",family="EB Garamond 08",
           label.padding = unit(c(3.25, 1.0, 3.0, 1.0), "lines"))+
  #caption
  annotate("richtext",x=Inf,y=-Inf,hjust=1,vjust=0,label="<span style='font-size:6pt'>redrawn by *@schochastics*</span>",
           size=8,fill=NA,family="EB Garamond 08",label.color=NA,
           label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines"))+
  theme_void()+
  theme(plot.title = element_text(family="EB Garamond 08",size=46,hjust=0,margin=margin(0,0,0,0)),
        plot.background = element_rect(fill="#f2eecb",colour="black",size=0.5),
        panel.background = element_rect(fill="#f2eecb",colour="black",size=0.1))

h <- 12
w <- h*1.496
ggsave("034-globa_ports.png",width = w,height = h,dpi = 300)


#plotrix arctext