library(jsonlite)
library(tidyverse)

item <- read_json("test.json")

lenitem <- length(item)


tvals <- sapply(1:lenitem,function(i) return(item[[i]]$time))
bytes <- as.numeric(sapply(1:lenitem,function(i) return(item[[i]]$interfaces[[3]][2][[1]]+item[[i]]$interfaces[[3]][10][[1]]+0.0)))

cpuidle <- as.numeric(sapply(1:lenitem,function(i) return(item[[i]]$cpu[[1]][5])))
cpusoftirq <- as.numeric(sapply(1:lenitem,function(i) return(item[[i]]$cpu[[1]][8])))
cpusys <- as.numeric(sapply(1:lenitem,function(i) return(item[[i]]$cpu[[1]][4])))

dat <- as_tibble(data.frame(t=(tvals-min(tvals))/1e9,xfer=bytes*8/1e6,cpusirq=cpusoftirq/100.0,cpuidle=cpuidle/100.0,cpusys=cpusys/100.0))

ggplot(dat) + geom_point(aes(t,xfer))
ggplot(dat) + geom_point(aes(t,cpuidle))
ggplot(dat) + geom_point(aes(t,cpusirq))
ggplot(dat) + geom_point(aes(t,cpusys))

## do some smoothing using GAM and predict sirq/xfer at 1 second intervals

smoothdat <- data.frame(t=seq(0,max(dat$t)),xfer=predict(gam(xfer ~ s(t,k=100),data=dat),data.frame(t=seq(0,max(dat$t)))),
                        cpusirq=predict(gam(cpusirq ~ s(t,k=100),data=dat),data.frame(t=seq(0,max(dat$t)))),
                        cpuidle=predict(gam(cpuidle ~ s(t,k=100),data=dat),data.frame(t=seq(0,max(dat$t)))),
                        cpusys=predict(gam(cpusys ~ s(t,k=100),data=dat),data.frame(t=seq(0,max(dat$t)))))


ggplot(smoothdat) + geom_point(aes(t,c(0,diff(xfer))))
ggplot(smoothdat) + geom_point(aes(t,c(0,diff(cpuidle))))
ggplot(smoothdat) + geom_point(aes(t,c(0,diff(cpusirq))))
ggplot(smoothdat) + geom_point(aes(t,c(0,diff(cpusys))))
ggplot(smoothdat,aes(c(0,diff(xfer)),c(0,diff(cpusirq)))) + geom_point()+coord_cartesian(xlim=c(0,60),ylim=c(0,1))+geom_smooth()+labs(x="Smoothed Transfer Estimate: Mbit/s",y="CPU SIRQ fraction")

ggsave("ExampPlot2.png")

ggplot(smoothdat,aes(c(0,diff(xfer)),c(0,diff(cpuidle)))) + geom_point()+coord_cartesian(xlim=c(0,60),ylim=c(0,1))+geom_smooth(method="lm")+labs(x="Smoothed Transfer Estimate: Mbit/s",y="CPU Idle fraction")

ggsave("ExampPlotIdle.png")
