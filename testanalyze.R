library(jsonlite)
library(tidyverse)
library(gam)

item <- read_json("testdata.json")

pingitem <- read_json("pingtest.json");

pinseqtime <- sapply(1:length(pingitem),function(x){return(pingitem[[x]]$time[[1]])})
pinseqms <- sapply(1:length(pingitem),function(x){return(pingitem[[x]]$ping[[1]])})

pingdat <- tibble(t=pinseqtime,ms=pinseqms)

lenitem <- length(item)

wanIface=8
lanIface=2

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
smoothdat <- data.frame(t=seq(0,max(dat$t)),xfer=predict(gam(xfer ~ s(t,df=100),data=dat),data.frame(t=seq(0,max(dat$t)))),
                        cpusirq=predict(gam(cpusirq ~ s(t,df=100),data=dat),data.frame(t=seq(0,max(dat$t)))),
                        cpuidle=predict(gam(cpuidle ~ s(t,df=100),data=dat),data.frame(t=seq(0,max(dat$t)))),
                        cpusys=predict(gam(cpusys ~ s(t,df=100),data=dat),data.frame(t=seq(0,max(dat$t)))),
                        cpuusr=predict(gam(cpuusr ~ s(t,df=100),data=dat),data.frame(t=seq(0,max(dat$t)))))

pingplot <- ggplot(pingdat,aes(t,ms))+geom_point()+labs(x="t",y="Ping (ms)",title="Ping vs Time")
bwplot <- ggplot(smoothdat,aes(t,c(0,diff(xfer))))+geom_point()+labs(x="t",y="Bandwidth (Mbps)",title="Bandwidth vs Time")
idleplot <- ggplot(smoothdat,aes(t,c(0,diff(cpuidle/cpucount))))+geom_point()+labs(x="t",y="CPU fraction idle",title="Idle vs Time")
grid.arrange(pingplot,bwplot,idleplot)

ggplot(smoothdat) + geom_point(aes(t,c(0,diff(xfer))))+coord_cartesian(ylim=c(0,1000))
ggplot(smoothdat) + geom_point(aes(t,c(0,diff(cpuidle/cpucount))))
ggplot(smoothdat) + geom_point(aes(t,c(0,diff(cpusirq/cpucount))))
ggplot(smoothdat) + geom_point(aes(t,c(0,diff(cpusys/cpucount))))
ggplot(smoothdat) + geom_point(aes(t,c(0,diff(cpuusr/cpucount))))

ggplot(smoothdat,aes(c(0,diff(xfer)),c(0,diff(cpusirq/cpucount)))) + geom_point()+coord_cartesian(xlim=c(0,1500),ylim=c(0,1))+geom_smooth()+labs(x="Smoothed Transfer Estimate: Mbit/s",y="CPU SIRQ fraction")

ggsave("ExampPlot2.png")

ggplot(smoothdat,aes(c(0,diff(xfer)),c(1,diff(cpuidle/cpucount)))) + geom_point()+coord_cartesian(xlim=c(0,1500),ylim=c(0,1))+geom_smooth(method="lm")+labs(x="Smoothed Transfer Estimate: Mbit/s",y="CPU Idle fraction")

ggsave("ExampPlotIdle.png")

ggplot(smoothdat,aes(c(0,diff(xfer)),c(0,diff(cpuusr/cpucount)))) + geom_point()+coord_cartesian(xlim=c(0,1500),ylim=c(0,1))+geom_smooth(method="lm")+labs(x="Smoothed Transfer Estimate: Mbit/s",y="CPU Usr fraction")
