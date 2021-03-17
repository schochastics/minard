library(ggforce)
library(tidyverse)

n <- 140
angle <- seq(0,2*pi,length.out = n)
dat <- tibble(x=cos(angle),y=sin(angle),angle,txt=sample(LETTERS,n,replace = TRUE))

ggplot(dat,aes(x,y))+geom_path()+geom_text(aes(label=txt,angle=I(angle*180/pi)))

# arctext
center <- c(0,0)
radius <- 0.1
x <- "Chicago"
xvec <- strsplit(x, "")[[1]]
lenx <- length(xvec)
xwidths <- 0.1 * strwidth(xvec)
charangles <- xwidths/radius
changrang <- range(charangles)
charangles[charangles < changrang[2]/2] <- changrang[2]/2
start <- pi/2 + sum(charangles)/2
charstart <- c(start, start - cumsum(charangles)[-lenx])
charpos <- charstart - charangles/2

xylim <- c(0,10,0,10)
plotdim <- c(8,6)
ymult <- (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
res <- tibble()
for (xchar in 1:lenx) {
  srt <- 180 * charpos[xchar]/pi - 90
  x <- center[1] + radius * cos(charpos[xchar])
  y <- center[2] + radius * sin(charpos[xchar]) * ymult
  t <- xvec[xchar]
  res <- bind_rows(res,tibble(x,y,t,srt))
}
ggplot(res,aes(x,y))+geom_text(aes(label=t,angle=I(srt)))+coord_fixed()
