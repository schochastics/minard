library(HistData)
library(tidyverse)
library(ggrepel)
library(patchwork)
library(extrafont)

loadfonts()
data("Minard.cities")
data("Minard.temp")
data("Minard.troops")

idx <- sapply(Minard.temp$long,function(x)which(abs(Minard.cities$long-x)==min(abs(Minard.cities$long-x))))
Minard.temp2 <- data.frame(long=rev(Minard.temp$long),
                           lat=c(54.4,54.3,54.4,54.1,54.2,54.6,54.8,55.2,55.7))

p1 <- ggplot()+
  geom_segment(data=Minard.temp2,aes(x=long,y=lat,xend=long,yend=53.2),size=0.2)+
  geom_path(data=Minard.troops,aes(x=long,y=lat,size=survivors,col=direction,group=group),
            lineend = "round", linejoin = "round")+
  scale_x_continuous(limits=c(24,38),name="")+
  scale_y_continuous(limits=c(53.2,56),name="")+
  scale_color_manual(values=c("#D7C181","#424242"))+
  scale_size(range=c(1,10),breaks = c(1e5,2e5,3e5))+
  geom_text_repel(data=Minard.cities,aes(long,lat,label=city),segment.alpha = 0,
                  family = "EB Garamond 08",size=4)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0,0,-0.7,0),"cm")
  )
p1
p2 <- ggplot(Minard.temp,aes(x=long,y=temp,label=date))+
  geom_line()+
  geom_text(family = "EB Garamond 08",size=4,vjust=1)+
  scale_x_continuous(limits=c(24,38),name="")+
  scale_y_continuous(limits=c(-31.5,0),position = "right",name="")+
  ggtitle(label ="GRAPHIC TABLE of the temperature in degrees below zero of the Réaumur thermometer")+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "EB Garamond 08"),
        panel.grid.major.y = element_line(color="black",size=0.1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(-1,0,0,0),"cm"),
        plot.title= element_text(hjust=0.5,size=12,family="EB Garamond 08")
        )+
  geom_segment(aes(xend=long,yend=0),size=0.2)

p2
descr <- expression(paste("Figurative Map", scriptstyle(" of the successive losses in men of the French Army in the Russian campaign 1812-1813.")))
p <- p1+p2+ plot_layout(ncol = 1, heights = c(3, 1))+
  plot_annotation(title=descr,
                  subtitle = "Drawn by M. Minard, Inspector General of Bridges and Roads (retired). Paris, November 20, 1869.",
                  caption = "redrawn by @schochastics",
                  theme = theme(plot.title=element_text(family="EB Garamond 08",size=20,face = "italic"),
                                plot.subtitle = element_text(family="EB Garamond 08",size=10,hjust = 0.5),
                                plot.caption = element_text(family="EB Garamond 08",colour="grey")))
p
ggsave("napoleon.png",p,width=12,height=5.8)
