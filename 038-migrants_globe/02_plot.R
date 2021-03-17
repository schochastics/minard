library(tidyverse)
library(ggraph)
library(igraph)
library(ggtext)

setwd("038-migrants_globe/")

center_NA <- c(-100.167,45.167)
center_SA <- c(-56.1004,-15.6006)
center_AF <- c(14.100526,1.616742)
center_AS <- c(76.08583,31.48333)
center_AU <- c(132.133,-25.117)

dict <- read_csv("data/dict.csv",
                 col_types = cols(
                   country = col_character(),
                   iso_a3 = col_character(),
                   continent = col_character()
                 ))

# countries <- c("United Kingdom","China","India","France","Germany","Republic of Congo")
countries <- c("United Kingdom","China","India","Nigeria","Brazil")
ctry_colors <- c("#86a873","#ffffbf","#997243","#1C86EE", "#CD6090", "#9F79EE")
iso <- dict$iso_a3[match(countries,dict$country)]

land <- sf::st_read("../000-data/naturalearth/ne_110m_land/ne_110m_land.shp")
land_clip <- sf::st_crop(land,c(xmin=-127.3,ymin=-57.8,xmax=175.7,ymax=75.8))

f <- list.files("results",pattern = "gtree_lst",full.names = TRUE)
f <- f[length(f)]
gtree_lst <- readRDS(f)

#combine network smartly manually
glist <- lapply(gtree_lst,igraph::as_data_frame,what="both")

verts <- map_dfr(1:length(glist), function(i) mutate(glist[[i]]$vertices,name=paste0(iso[i],"_",name),ctry=iso[i]))
edges <- map_dfr(1:length(glist), function(i) mutate(glist[[i]]$edges,
                                                     from=paste0(iso[i],"_",from),
                                                     to=paste0(iso[i],"_",to),
                                                     ctry=iso[i]))
verts <- verts %>% select(name,ctry,tnss,x,y)
gtree <- graph_from_data_frame(edges,F,verts)


el <- get.edgelist(gtree)
idl <- which(!str_detect(el[,1],paste0(c("dummy",countries),collapse="|")) | !str_detect(el[,2],paste0(c("dummy",countries),collapse="|")))
E(gtree)$label <- ""
E(gtree)$label[idl] <- scales::label_number_si(accuracy = 0.1)(E(gtree)$flow[idl])
E(gtree)$flow_norm <- ambient::normalise(E(gtree)$flow,to=c(1,6))

map_title <- "<span style='font-size:32pt'>FIGURATIVE MAP representing<br>EMIGRANTS around the WORLD (2005-2010)</span>"
ctry_labels <- str_pad(str_replace_all(countries,"^|$","\\*"),width = 25,side = "right",pad = ".")

ggraph(gtree,"manual",x=V(gtree)$x,y=V(gtree)$y)+
  geom_sf(data=land_clip,fill="#e8e4c9",colour="black",size=0.3)+
  geom_edge_link(aes(filter=ctry==iso[1],width=I(flow_norm+0.3),label=label),edge_color="black",family = "EB Garamond 08",label_size=2,
                   lineend = "round",show.legend = FALSE)+
  geom_edge_link(aes(filter=ctry==iso[1],width=I(flow_norm),label=label),edge_color=ctry_colors[1],family = "EB Garamond 08",label_size=2,
                     lineend = "round",show.legend = FALSE)+
  geom_edge_link(aes(filter=ctry==iso[2],width=I(flow_norm+0.3),label=label),edge_color="black",family = "EB Garamond 08",label_size=2,
                 lineend = "round",show.legend = FALSE)+
  geom_edge_link(aes(filter=ctry==iso[2],width=I(flow_norm),label=label),edge_color=ctry_colors[2],family = "EB Garamond 08",label_size=2,
                 lineend = "round",show.legend = FALSE)+
  geom_edge_link(aes(filter=ctry==iso[3],width=I(flow_norm+0.3),label=label),edge_color="black",family = "EB Garamond 08",label_size=2,
                 lineend = "round",show.legend = FALSE)+
  geom_edge_link(aes(filter=ctry==iso[3],width=I(flow_norm),label=label),edge_color=ctry_colors[3],family = "EB Garamond 08",label_size=2,
                 lineend = "round",show.legend = FALSE)+
  geom_edge_link(aes(filter=ctry==iso[4],width=I(flow_norm+0.3),label=label),edge_color="black",family = "EB Garamond 08",label_size=2,
                 lineend = "round",show.legend = FALSE)+
  geom_edge_link(aes(filter=ctry==iso[4],width=I(flow_norm),label=label),edge_color=ctry_colors[4],family = "EB Garamond 08",label_size=2,
                 lineend = "round",show.legend = FALSE)+
  geom_edge_link(aes(filter=ctry==iso[5],width=I(flow_norm+0.3),label=label),edge_color="black",family = "EB Garamond 08",label_size=2,
                 lineend = "round",show.legend = FALSE)+
  geom_edge_link(aes(filter=ctry==iso[5],width=I(flow_norm),label=label),edge_color=ctry_colors[5],family = "EB Garamond 08",label_size=2,
                 lineend = "round",show.legend = FALSE)+
  # geom_edge_link(aes(filter=ctry==iso[6],width=I(flow_norm+0.3),label=label),edge_color="black",family = "EB Garamond 08",label_size=2,
  #                lineend = "round",show.legend = FALSE)+
  # geom_edge_link(aes(filter=ctry==iso[6],width=I(flow_norm),label=label),edge_color=ctry_colors[6],family = "EB Garamond 08",label_size=2,
  #                lineend = "round",show.legend = FALSE)+
  geom_node_point(aes(fill=ctry),shape=21,alpha=0)+
  scale_fill_manual(values=ctry_colors,labels=ctry_labels,name="**Emigrating from**")+
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
  theme(legend.position = c(1,1),
        legend.text = element_markdown(family="Xanh Mono",size=24),
        legend.title = element_markdown(family="EB Garamond 08",size=30),
        legend.justification = c(1,1), legend.margin=margin(c(25,25,25,25), "pt"),
        legend.background = element_rect(colour="black",fill="#f2eecb"),
        plot.title = element_text(family="EB Garamond 08",size=46,hjust=0,margin=margin(0,0,0,0)),
        plot.background = element_rect(fill="#f2eecb",colour="black",size=0.5),
        panel.background = element_rect(fill="#f2eecb",colour="black",size=0.1))+
  guides(fill = guide_legend(label.position = "left",override.aes = list(size=14,alpha=1,shape=22),
                             title.position = "left")) 

h <- 20
w <- h*1.28976

idf <- str_extract(f,"[0-9]{3,3}")
ggsave(paste0("038-migrants_globe_",idf,".png"),width = w,height = h,dpi = 300)

#smoothed
smooth_lst <- vector("list",length(iso))
for(i in 1:length(iso)){
  gtree_lst[[i]] <- netUtils::delete_isolates(gtree_lst[[i]])
  smooth_lst[[i]] <- edgebundle::tnss_smooth(netUtils::delete_isolates(gtree_lst[[i]]),bw=15,n=50) %>% 
    mutate(ctry=iso[i],destination=paste0(ctry,"_",destination)) 
}

smooth_tbl <- do.call("bind_rows",smooth_lst) %>% 
  mutate(flow=ambient::normalise(flow,to=c(1,6)))

dest_tbl <- igraph::as_data_frame(gtree,"vertices") %>% 
  dplyr::filter(tnss!="dummy",tnss!="root") %>%
  mutate(name=str_remove_all(name,"[A-Z]{2,3}_")) %>% 
  select(name,x,y) %>% 
  distinct(name,.keep_all = TRUE)

ggraph(gtree,"manual",x=V(gtree)$x,y=V(gtree)$y)+
  geom_sf(data=land_clip,fill="#e8e4c9",colour="black",size=0.3)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[1],],aes(x,y,size=I(flow+0.3),group=destination),lineend = "round",col="black",show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[1],],aes(x,y,size=I(flow),group=destination),lineend = "round",col=ctry_colors[1],show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[2],],aes(x,y,size=I(flow+0.3),group=destination),lineend = "round",col="black",show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[2],],aes(x,y,size=I(flow),group=destination),lineend = "round",col=ctry_colors[2],show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[3],],aes(x,y,size=I(flow+0.3),group=destination),lineend = "round",col="black",show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[3],],aes(x,y,size=I(flow),group=destination),lineend = "round",col=ctry_colors[3],show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[4],],aes(x,y,size=I(flow+0.3),group=destination),lineend = "round",col="black",show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[4],],aes(x,y,size=I(flow),group=destination),lineend = "round",col=ctry_colors[4],show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[5],],aes(x,y,size=I(flow+0.3),group=destination),lineend = "round",col="black",show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[5],],aes(x,y,size=I(flow),group=destination),lineend = "round",col=ctry_colors[5],show.legend = FALSE)+
  # geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[6],],aes(x,y,size=I(flow+0.3),group=destination),lineend = "round",col="black",show.legend = FALSE)+
  # geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[6],],aes(x,y,size=I(flow),group=destination),lineend = "round",col=ctry_colors[6],show.legend = FALSE)+
  # 
  geom_node_point(aes(fill=ctry),shape=21,alpha=0)+
  geom_text(data=dest_tbl,aes(x,y,label=name),family="EB Garamond 08",size=3)+
  scale_fill_manual(values=ctry_colors,labels=ctry_labels,name="**Emigrating from**")+
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
  theme(legend.position = c(1,1),
        legend.text = element_markdown(family="Xanh Mono",size=24),
        legend.title = element_markdown(family="EB Garamond 08",size=30),
        legend.justification = c(1,1), legend.margin=margin(c(25,25,25,25), "pt"),
        legend.background = element_rect(colour="black",fill="#f2eecb"),
        plot.title = element_text(family="EB Garamond 08",size=46,hjust=0,margin=margin(0,0,0,0)),
        plot.background = element_rect(fill="#f2eecb",colour="black",size=0.5),
        panel.background = element_rect(fill="#f2eecb",colour="black",size=0.1))+
  guides(fill = guide_legend(label.position = "left",override.aes = list(size=14,alpha=1,shape=22),
                             title.position = "left")) 

ggsave(paste0("038-migrants_globe_smoothed_",idf,".png"),width = w,height = h,dpi = 300)

#animated
library(gganimate)
smooth_tbl <- smooth_tbl %>% group_by(destination) %>% mutate(time=row_number()) %>% ungroup()

p <- ggplot()+
  geom_sf(data=land_clip,fill="#e8e4c9",colour="black",size=0.3)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[1],],aes(x,y,size=I(flow+0.3),group=destination),lineend = "round",col="black",show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[1],],aes(x,y,size=I(flow),group=destination),lineend = "round",col=ctry_colors[1],show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[2],],aes(x,y,size=I(flow+0.3),group=destination),lineend = "round",col="black",show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[2],],aes(x,y,size=I(flow),group=destination),lineend = "round",col=ctry_colors[2],show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[3],],aes(x,y,size=I(flow+0.3),group=destination),lineend = "round",col="black",show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[3],],aes(x,y,size=I(flow),group=destination),lineend = "round",col=ctry_colors[3],show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[4],],aes(x,y,size=I(flow+0.3),group=destination),lineend = "round",col="black",show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[4],],aes(x,y,size=I(flow),group=destination),lineend = "round",col=ctry_colors[4],show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[5],],aes(x,y,size=I(flow+0.3),group=destination),lineend = "round",col="black",show.legend = FALSE)+
  geom_path(data=smooth_tbl[smooth_tbl$ctry==iso[5],],aes(x,y,size=I(flow),group=destination),lineend = "round",col=ctry_colors[5],show.legend = FALSE)+
  geom_text(data=dest_tbl,aes(x,y,label=name),family="EB Garamond 08",size=3)+
  scale_fill_manual(values=ctry_colors,labels=ctry_labels,name="**Emigrating from**")+
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
  #caption
  annotate("richtext",x=Inf,y=-Inf,hjust=1,vjust=0,label="<span style='font-size:6pt'>redrawn by *@schochastics*</span>",
           size=8,fill=NA,family="EB Garamond 08",label.color=NA,
           label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines"))+
  theme_void()+
  theme(legend.position = c(1,1),
        legend.text = element_markdown(family="Xanh Mono",size=24),
        legend.title = element_markdown(family="EB Garamond 08",size=30),
        legend.justification = c(1,1), legend.margin=margin(c(25,25,25,25), "pt"),
        legend.background = element_rect(colour="black",fill="#f2eecb"),
        plot.title = element_text(family="EB Garamond 08",size=46,hjust=0,margin=margin(0,0,0,0)),
        plot.background = element_rect(fill="#f2eecb",colour="black",size=0.5),
        panel.background = element_rect(fill="#f2eecb",colour="black",size=0.1))+
  guides(fill = guide_legend(label.position = "left",override.aes = list(size=14,alpha=1,shape=22),
                             title.position = "left")) +
  transition_reveal(time)

pa <- animate(p,height = h*100, width =w*100)
anim_save("animated.gif",pa)
#all migration
dat <- readRDS("results/bundling_all.RDS")

hbundle <- dat$hbundle
fbundle <- dat$fbundle

ggplot()+
  geom_sf(data=land,fill="#e8e4c9",colour="black",size=0.2)+
  geom_path(data = hbundle,aes(x,y,group=group,size=I(size)),col="black",alpha=0.5)+
  geom_path(data = hbundle,aes(x,y,group=group,size=I(size/10)),col="#D7C181")+
  theme_graph(background = "#f2eecb")+
  coord_sf(ylim = range(-55,90),clip = "on")

ggplot()+
  geom_sf(data=land,fill="#e8e4c9",colour="black",size=0.2)+
  geom_path(data = fbundle,aes(x,y,group=group,size=I(size)),col="black",alpha=0.5)+
  geom_path(data = fbundle,aes(x,y,group=group,size=I(size/10)),col="#D7C181")+
  theme_graph(background = "#f2eecb")+
  coord_sf(ylim = range(-55,90),clip = "on")+
  theme(plot.title = element_text(family="EB Garamond 08",size=46,hjust=0,margin=margin(0,0,0,0)))+
  labs(title="Global Migration 2005-2010")
  
ggsave("038-migrants_globe_all.png",width = w,height = h,dpi = 300)
