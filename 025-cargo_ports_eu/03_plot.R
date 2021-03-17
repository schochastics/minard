library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggtext)

dat <- read_csv("data/cargo2019.csv")%>% dplyr::filter(total>10000)
dat$city <- dat$city %>% str_remove_all(" \\(.*\\)")
europe <- ne_countries(scale=50,continent = "Europe",returnclass = "sf")

set.seed(1223)
europe$fill <- sample(LETTERS[1:4],nrow(europe),replace = TRUE)
dat$size <- ambient::normalise(log(dat$total),to=c(2,8))
dat$radius <- dat$size/10

newloc <- packcircles::circleRepelLayout(select(dat,long,lat,radius))
dat1 <- bind_cols(dat,newloc$layout[,-3])


map_title <- "<span style='font-size:24pt;'>Figurative Map of the Cargo Tonnage of the Major Ports of Europe</span>"
ggplot()+
  geom_sf(data=europe,aes(fill=fill),show.legend = FALSE,size=0.1)+
  geom_sf_text(data=europe[europe$pop_est>8e6,],aes(label=name_long),family="EB Garamond 08")+
  geom_point(data=dat1,aes(x,y,size=I(size)),show.legend = FALSE,shape=21,fill="darkgreen")+
  geom_text(data=dat1,aes(x,y,size=I(size/3),label=total),family="EB Garamond 08")+
  geom_text(data=dat1,aes(x,y,label=city),size=2,vjust=-dat$size/3,family="EB Garamond 08")+
  scale_fill_manual(values=c("#F9D8D6","#CDF5F6","#F9EBDF","#EFD2AC"))+
  coord_sf(expand=FALSE,xlim = c(-10,50),ylim=c(35,75))+
  #title
  annotate("richtext",x=-Inf,y=Inf,hjust=0,vjust=1,label=map_title,
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

h <- 17
w <- h*1.25745

ggsave("025-cargo_ports_eu.png",width = w,height = h,dpi = 300)
