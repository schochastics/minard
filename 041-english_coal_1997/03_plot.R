library(tidyverse)
library(ggraph)
library(igraph)
library(ggtext)

setwd("041-english_coal_1997/")

f <- list.files("results",pattern = "gtree",full.names = TRUE)
f <- f[length(f)]


center_NA <- c(-100.167,45.167)
center_SA <- c(-56.1004,-15.6006)
center_AF <- c(14.100526,1.616742)
center_AS <- c(76.08583,38.48333)
center_AU <- c(132.133,-25.117)

land <- sf::st_read("../000-data/naturalearth/ne_110m_land/ne_110m_land.shp")
land_clip <- sf::st_crop(land,c(xmin=-127.3,ymin=-57.8,xmax=155.7,ymax=66.8))

gtree_lst <- readRDS(f)
gtree <- graph_union(gtree_lst)
el <- get.edgelist(gtree)
idl <- which(!str_detect(el[,1],"dummy|United Kingdom") | !str_detect(el[,2],"dummy|United Kingdom"))
E(gtree)$label <- ""
E(gtree)$label[idl] <- scales::label_number_si(accuracy = 0.1)(E(gtree)$flow[idl])
E(gtree)$flow <- ambient::normalise(E(gtree)$flow,to=c(1,6))

# graph ----
map_title <- "<span style='font-size:24pt'>FIGURATIVE MAP of *ENGLISH COAL EXPORTS in 1997*</span>"

ggraph(gtree,"manual",x=V(gtree)$x,y=V(gtree)$y)+
  geom_sf(data=land_clip,fill="#e8e4c9",colour="black",size=0.3)+
  geom_edge_link(aes(width=I(flow+0.3),label=label),edge_color="black",family = "EB Garamond 08",label_size=2,
                 lineend = "round",show.legend = FALSE)+
  geom_edge_link(aes(width=I(flow),label=label),edge_color="#86A873",family = "EB Garamond 08",label_size=2,
                 lineend = "round",show.legend = FALSE)+
  geom_node_text(aes(filter=tnss=="leaf",label=name),
                 family = "EB Garamond 08",size=2,repel = TRUE)+
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
  annotate("richtext",x=Inf,y=Inf,hjust=1,vjust=1,label=map_title,
           size=8,fill="#e8e4c9",family="EB Garamond 08",
           label.padding = unit(c(3.25, 1.0, 3.0, 1.0), "lines"))+
  #caption
  annotate("richtext",x=Inf,y=-Inf,hjust=1,vjust=0,label="<span style='font-size:6pt'>redrawn by *@schochastics*</span>",
           size=8,fill=NA,family="EB Garamond 08",label.color=NA,
           label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines"))+
  theme_void()+
  theme(plot.title = element_text(family="EB Garamond 08",size=46,hjust=0,margin=margin(0,0,0,0)),
        plot.background = element_rect(fill="#e8e4c9",colour="black",size=0.5),
        panel.background = element_rect(fill="#e8e4c9",colour="black",size=0.1))

h <- 20
w <- h*1.28976

idf <- str_extract(f,"[0-9]{3,3}")
ggsave(paste0("041-english_coal_",idf,".png"),width = w,height = h,dpi = 300)


#smoothed
smooth_df <- edgebundle::tnss_smooth(gtree,bw=15,n=20)
iddest <- sort(unique(smooth_df$destination))
ocean <- iddest[1:sum(V(gtree_lst$ocean)$tnss=="leaf")]
land <- setdiff(iddest,ocean)
ggraph(gtree,"manual",x=V(gtree)$x,y=V(gtree)$y)+
  geom_path(data=subset(smooth_df,destination%in%ocean),aes(x,y,size=I(flow+0.3),group=destination),lineend="round",col="black",show.legend = FALSE)+
  geom_path(data=subset(smooth_df,destination%in%ocean),aes(x,y,size=I(flow),group=destination),lineend="round",col="#86A873",show.legend = FALSE)+
  geom_sf(data=land_clip,fill="#e8e4c9",colour="black",size=0.3)+
  geom_path(data=subset(smooth_df,destination%in%land),aes(x,y,size=I(flow+0.3),group=destination),lineend="round",col="black",show.legend = FALSE)+
  geom_path(data=subset(smooth_df,destination%in%land),aes(x,y,size=I(flow),group=destination),lineend="round",col="#86A873",show.legend = FALSE)+
  geom_node_text(aes(filter=tnss=="leaf",label=name),
                 family = "EB Garamond 08",size=2,repel = TRUE)+
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
  annotate("richtext",x=Inf,y=Inf,hjust=1,vjust=1,label=map_title,
           size=8,fill="#e8e4c9",family="EB Garamond 08",
           label.padding = unit(c(3.25, 1.0, 3.0, 1.0), "lines"))+
  #caption
  annotate("richtext",x=Inf,y=-Inf,hjust=1,vjust=0,label="<span style='font-size:6pt'>redrawn by *@schochastics*</span>",
           size=8,fill=NA,family="EB Garamond 08",label.color=NA,
           label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines"))+
  theme_void()+
  theme(plot.title = element_text(family="EB Garamond 08",size=46,hjust=0,margin=margin(0,0,0,0)),
        plot.background = element_rect(fill="#e8e4c9",colour="black",size=0.5),
        panel.background = element_rect(fill="#e8e4c9",colour="black",size=0.1))

ggsave(paste0("041-english_coal_smoothed_",idf,".png"),width = w,height = h,dpi = 300)

