library(jsonlite)
library(tidyverse)

item <- read_json("test-pico.json")

lenitem <- length(item)

wanIface=4

cpucount = length(item[[1]]$cpu)-1


tvals <- sapply(1:lenitem,function(i) return(item[[i]]$time))
bytes <- as.numeric(sapply(1:lenitem,function(i) return(item[[i]]$interfaces[[wanIface]][2][[1]]+item[[i]]$interfaces[[wanIface]][10][[1]]+0.0)))

cpuidle <- as.numeric(sapply(1:lenitem,function(i) return(item[[i]]$cpu[[1]][5])))
cpusoftirq <- as.numeric(sapply(1:lenitem,function(i) return(item[[i]]$cpu[[1]][8])))
cpusys <- as.numeric(sapply(1:lenitem,function(i) return(item[[i]]$cpu[[1]][4])))
cpuusr <- as.numeric(sapply(1:lenitem,function(i) return(item[[i]]$cpu[[1]][2])))

dat <- as_tibble(data.frame(t=(tvals-min(tvals)),xfer=bytes*8/1e6,
                            cpusirq=cpusoftirq/100.0,cpuidle=cpuidle/100.0,cpusys=cpusys/100.0,
                            cpuusr=cpuusr/100.0))

ggplot(dat) + geom_point(aes(t,xfer))
ggplot(dat) + geom_point(aes(t,cpuidle))
ggplot(dat) + geom_point(aes(t,cpusirq))
ggplot(dat) + geom_point(aes(t,cpusys))

## do some smoothing using GAM and predict sirq/xfer at 1 second intervals

smoothdat <- data.frame(t=seq(0,max(dat$t)),xfer=predict(gam(xfer ~ s(t,k=100),data=dat),data.frame(t=seq(0,max(dat$t)))),
                        cpusirq=predict(gam(cpusirq ~ s(t,k=100),data=dat),data.frame(t=seq(0,max(dat$t)))),
                        cpuidle=predict(gam(cpuidle ~ s(t,k=100),data=dat),data.frame(t=seq(0,max(dat$t)))),
                        cpusys=predict(gam(cpusys ~ s(t,k=100),data=dat),data.frame(t=seq(0,max(dat$t)))),
                        cpuusr=predict(gam(cpuusr ~ s(t,k=100),data=dat),data.frame(t=seq(0,max(dat$t)))))


ggplot(smoothdat) + geom_point(aes(t,c(0,diff(xfer))))+coord_cartesian(ylim=c(0,1000))
ggplot(smoothdat) + geom_point(aes(t,c(0,diff(cpuidle/cpucount))))
ggplot(smoothdat) + geom_point(aes(t,c(0,diff(cpusirq/cpucount))))
ggplot(smoothdat) + geom_point(aes(t,c(0,diff(cpusys/cpucount))))
ggplot(smoothdat) + geom_point(aes(t,c(0,diff(cpuusr/cpucount))))
ggplot(smoothdat,aes(c(0,diff(xfer)),c(0,diff(cpusirq/cpucount)))) + geom_point()+coord_cartesian(xlim=c(0,1500),ylim=c(0,1))+geom_smooth()+labs(x="Smoothed Transfer Estimate: Mbit/s",y="CPU SIRQ fraction")

ggsave("ExampPlot2.png")

ggplot(smoothdat,aes(c(0,diff(xfer)),c(1,diff(cpuidle/cpucount)))) + geom_point()+coord_cartesian(xlim=c(0,1500),ylim=c(0,1))+geom_smooth(method="lm")+labs(x="Smoothed Transfer Estimate: Mbit/s",y="CPU Idle fraction")

ggsave("ExampPlotIdle.png")
