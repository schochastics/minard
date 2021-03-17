#https://ourworldindata.org/grapher/coal-production-by-country
library(tidyverse)
library(patchwork)
library(ggtext)

setwd("054-coal_production/")
coal <- read_csv("data/coal-production-by-country.csv")
countries <- c("China","United States","India","Russia","Germany","United Kingdom")
ctry_colors <- c("#86a873","#ffffbf","#7795d3","#c4bbb2", "#CD6090", "#a68ed9")

coal <- coal %>% 
  dplyr::filter(Entity%in%countries) %>% 
  dplyr::filter(Year>=1981) %>% 
  rename(TWh=4) %>% 
  mutate(Entity=factor(Entity,levels = countries))

coal_per_year <- coal %>% 
  group_by(Year) %>% 
  dplyr::summarise(total=sum(TWh),.groups="drop")

lower_seq <- seq(3000,min(coal_per_year$total),3000)
upper_seq <- seq(max(lower_seq)+3000,30000,3000)

delims <- coal_per_year %>% 
  rowwise() %>% 
  mutate(lim=sum(total>upper_seq)) %>% 
  group_by(lim) %>% 
  slice(1) %>% 
  ungroup() %>% 
  dplyr::filter(lim>0) %>% 
  mutate(lim=upper_seq[lim]) %>% 
  select(-total) %>% 
  bind_rows(tibble(Year=1981,lim=lower_seq))

annots <- tibble(country=countries,x=c(2005,2005,2005,2005,2005,1989),
                                   y=c(16500,7500,3700,1500,300,100))

xlab_year_raw <- as.character(1981:2019)
xlab_year <- ifelse(str_sub(xlab_year_raw,-1)=="0",xlab_year_raw,str_sub(xlab_year_raw,3,4))
xlab_year2 <- str_sub(xlab_year_raw,3,4)
xlab_year2[seq(2,length(xlab_year2),2)] <- ""

p1 <- ggplot(coal)+
  geom_col(aes(x=Year,y=TWh,fill=Entity),col="black",size=0.3,width=1)+
  geom_segment(data=delims,aes(x=Year-0.5,y=lim,yend=lim),xend=2019+0.5,size=0.3)+
  geom_text(data=annots,aes(x,y,label=country),family="EB Garamond 08",vjust=0,size=8)+
  scale_fill_manual(values = ctry_colors)+
  scale_x_continuous(breaks=1981:2019,labels = xlab_year2,expand = c(0.01,0.01))+
  scale_y_continuous(limits=c(0,35000),breaks = seq(3e3,3e4,3e3), labels = paste0(seq(3e3,3e4,3e3)," TWh"),
                     expand = c(0,0),position = "right")+
  labs(x="",y="")+
  annotate("text",x=1990,y=26000,label="Table No. 1",family="EB Garamond 08",size=10)+
  theme(legend.position = "none", aspect.ratio = 2,
        axis.text = element_text(family = "EB Garamond 08",size=14),
        panel.grid = element_blank(),
        panel.background = element_rect(colour=NA,fill="#f2eecb"),
        plot.background = element_rect(fill="#f2eecb",colour=NA,size=0.5),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0,0,0,0),"cm"))

delims1 <- coal %>% 
  group_by(Year) %>% 
  top_n(1,TWh) %>% 
  ungroup()

all_seq <- seq(3e3,23e3,3e3)
delims2 <- delims1 %>% 
  rowwise() %>% 
  mutate(lim=sum(TWh>all_seq)) %>% 
  group_by(lim) %>% 
  slice(1) %>% 
  ungroup() %>% 
  dplyr::filter(lim>0) %>% 
  mutate(lim=all_seq[lim])

p2 <- ggplot(coal)+
  geom_line(aes(x=Year,y=TWh,col=Entity),size=1.5)+
  geom_line(aes(x=Year,y=TWh+0.7,group=Entity),size=0.5)+
  geom_segment(data=delims1,aes(x=Year,xend=Year,y=TWh),yend=0,size=0.3)+
  geom_segment(data=delims2,aes(x=Year,y=lim,yend=lim),xend=2019,size=0.3)+
  # geom_text(data=annots,aes(x,y,label=country),family="EB Garamond 08",vjust=0)+
  scale_color_manual(values = ctry_colors)+
  scale_x_continuous(breaks=1981:2019,labels = xlab_year,expand = c(0.01,0.01))+
  scale_y_continuous(limits=c(0,23000),breaks = all_seq,labels = paste0(all_seq," TWh"),
                     expand = c(0,0),position = "right")+
  labs(x="",y="")+
  annotate("text",x=1990,y=17150,label="Table No. 2",family="EB Garamond 08",size=10)+
  theme(legend.position = "none", aspect.ratio = 2,
        axis.text = element_text(family = "EB Garamond 08",size=14),
        panel.grid = element_blank(),
        panel.background = element_rect(colour=NA,fill="#f2eecb"),
        plot.background = element_rect(fill="#f2eecb",colour=NA,size=0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=90,hjust=1),
        plot.margin = unit(c(0,0,0,0),"cm"))

graph_title <- "Graphical <span style='font-size:44pt'>Table</span> of <span style='font-size:44pt'>coal production</span> from 1900 to 2019"

p <- p1+p2+plot_annotation(title=graph_title,
                           caption = "<span style='font-size:6pt'>redrawn by *@schochastics*</span>",
                           theme = theme(
                             plot.caption = element_markdown(family="EB Garamond 08",size=12),
                             plot.title = element_markdown(family="EB Garamond 08",size=32),
                             plot.background = element_rect(colour="black",fill="#f2eecb")))
p
h <- 20
w <- h*0.678363

ggsave("054-coal_production.png",width = w,height = h,dpi = 300)
