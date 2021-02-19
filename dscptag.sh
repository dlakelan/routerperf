## this is *sourced* from the main script so that it inherits the
## shell variables from that script

ipt64dscp(){
    iptables -t mangle -A dscptag $*
    ip6tables -t mangle -A dscptag $*
}
ipt4dscp() {
    iptables -t mangle -A dscptag $*
}
ipt6dscp() {
    ip6tables -t mangle -A dscptag $*
}


## downgrade torrents etc UDP:
ipt64dscp -p udp -m multiport --sports "$UDPBULKPT" -j DSCP --set-dscp-class CS1
ipt64dscp -p udp -m multiport --dports "$UDPBULKPT" -j DSCP --set-dscp-class CS1

## downgrade torrents etc TCP:
ipt64dscp -p tcp -m multiport --sports "$TCPBULKPT" -j DSCP --set-dscp-class CS1
ipt64dscp -p tcp -m multiport --dports "$TCPBULKPT" -j DSCP --set-dscp-class CS1




## allow up to 5 binary divisions of ack packets (32x reduction). This
## still leads to about 1300 acks/second for a full gigabit download
## on one stream, but it always gives each stream at least 100
## acks/second which should normally be plenty? That's one ack every
## 10ms, but if not we can maybe tune this up a little

ackrate=300

ipt64dscp -p tcp -m tcp --tcp-flags ACK ACK -o $WAN -m length --length 1:100 -m hashlimit --hashlimit-mode srcip,srcport,dstip,dstport --hashlimit-name ackfilter1 --hashlimit-above "${ackrate}/second" --hashlimit-burst $ackrate --hashlimit-rate-match --hashlimit-rate-interval 1 -m statistic --mode random --probability .5 -j DROP
ipt64dscp -p tcp -m tcp --tcp-flags ACK ACK -o $WAN -m length --length 1:100 -m hashlimit --hashlimit-mode srcip,srcport,dstip,dstport --hashlimit-name ackfilter2 --hashlimit-above "$((ackrate*2))/second" --hashlimit-burst $ackrate --hashlimit-rate-match --hashlimit-rate-interval 1 -m statistic --mode random --probability .5 -j DROP
ipt64dscp -p tcp -m tcp --tcp-flags ACK ACK -o $WAN -m length --length 1:100 -m hashlimit --hashlimit-mode srcip,srcport,dstip,dstport --hashlimit-name ackfilter3 --hashlimit-above "$((ackrate*4))/second" --hashlimit-burst $ackrate --hashlimit-rate-match --hashlimit-rate-interval 1 -m statistic --mode random --probability .5 -j DROP
ipt64dscp -p tcp -m tcp --tcp-flags ACK ACK -o $WAN -m length --length 1:100 -m hashlimit --hashlimit-mode srcip,srcport,dstip,dstport --hashlimit-name ackfilter4 --hashlimit-above "$((ackrate*8))/second" --hashlimit-burst $ackrate --hashlimit-rate-match --hashlimit-rate-interval 1 -m statistic --mode random --probability .5 -j DROP
ipt64dscp -p tcp -m tcp --tcp-flags ACK ACK -o $WAN -m length --length 1:100 -m hashlimit --hashlimit-mode srcip,srcport,dstip,dstport --hashlimit-name ackfilter5 --hashlimit-above "$((ackrate*16))/second" --hashlimit-burst $ackrate --hashlimit-rate-match --hashlimit-rate-interval 1 -m statistic --mode random --probability .5 -j DROP




## boost jitsi meet udp to CS4, if you have the bandwidth you can
## boost these video conferences to CS5 and make it realtime, but then
## it can interfere with other realtime/game. Often CS4 will be enough

ipt64dscp -p udp --dport 10000 -j DSCP --set-dscp-class CS4
ipt64dscp -p udp --sport 10000 -j DSCP --set-dscp-class CS4

## boost zoom to CS4
ipt64dscp -p udp -m multiport --sports 3478:3479,8801:8802 -j DSCP --set-dscp-class CS4
ipt64dscp -p udp -m multiport --dports 3478:3479,8801:8802 -j DSCP --set-dscp-class CS4

## boost google meet CS4
ipt64dscp -p udp -m multiport --sports 19302:19309 -j DSCP --set-dscp-class CS4
ipt64dscp -p udp -m multiport --dports 19302:19309 -j DSCP --set-dscp-class CS4

## boost webex to CS4

ipt64dscp -p udp --dport 9000 -j DSCP --set-dscp-class CS4
ipt64dscp -p udp --sport 9000 -j DSCP --set-dscp-class CS4

## boost teamviewer to CS4

ipt64dscp -p udp --dport 5938 -j DSCP --set-dscp-class CS4
ipt64dscp -p udp --sport 5938 -j DSCP --set-dscp-class CS4
ipt64dscp -p tcp --dport 5938 -j DSCP --set-dscp-class CS4
ipt64dscp -p tcp --sport 5938 -j DSCP --set-dscp-class CS4


## boost DNS traffic
ipt4dscp -p udp --dport 53 -j DSCP --set-dscp-class CS4
ipt4dscp -p udp --sport 53 -j DSCP --set-dscp-class CS4


## boost the gaming machines UDP always to CS5 for realtime access if
## you have a low total bandwidth so that game/total is definitely
## above say 0.2, you might prefer to set CS4 here and use a
## link-share class, which will have a bit more jitter, but may enable
## you to drain backlogs faster


for dir in src dst ; do 
    ipt4dscp -p udp -m set --match-set "realtimeset4" $dir -j DSCP --set-dscp-class CS5
    ipt6dscp -p udp -m set --match-set "realtimeset6" $dir -j DSCP --set-dscp-class CS5
done

## implement intentional packet loss

if [ $pktlossp != "none" ] ; then
    ipt4dscp -p udp -m set --match-set "realtimeset4" src -m statistic --mode random --probability $pktlossp -j DROP
    ipt6dscp -p udp -m set --match-set "realtimeset6" src -m statistic --mode random --probability $pktlossp -j DROP
fi


#down prioritize low priority machines, tcp and udp:

for proto in udp tcp ; do
    for dir in src dst ; do 
	ipt4dscp -p $proto -m set --match-set "lowprioset4" $dir -j DSCP --set-dscp-class CS2
	ipt6dscp -p $proto -m set --match-set "lowprioset6" $dir -j DSCP --set-dscp-class CS2
    done
done


## downgrade UDP tagged CS5 that sends more than 450 pps (seems
## unlikely to be gaming traffic, more likely QUIC), comment this out
## if you want, or change to CS1 to further down-priority

# if your game "likes" when you have low bandwidth, this can cause you
# to "escape" the bandwidth limits, so comment it out

ipt4dscp -p udp -m dscp --dscp-class CS5 -m hashlimit --hashlimit-mode srcip,srcport,dstip,dstport --hashlimit-name udpbulk4 --hashlimit-above 450/second --hashlimit-burst 50 --hashlimit-rate-match --hashlimit-rate-interval 1 -j DSCP --set-dscp-class CS2


## movie streaming such as DASH protocol opens a connection and jams
## packets in it and then closes it, this causes potentially big
## problems on slower lines where it might take 100% of download for a
## few hundred milliseconds, but we can identify it because they're
## short lived...

## on the other hand, this might also cause problems for browsing as
## it will down-prioritize the first few seconds of transfer ... it's
## experimental

# down prioritize downloads with less than 500ms of transfer to CS2
ipt64dscp -p tcp -m connbytes --connbytes 0:$((DOWNRATE*500/8)) --connbytes-dir reply --connbytes-mode bytes -j DSCP --set-dscp-class CS2

# big transfers can be identified by their transferred bytes:

# down prioritize downloads that have transferred more than 10 seconds worth of packets
ipt64dscp -p tcp -m connbytes --connbytes $((DOWNRATE*10000/8)) --connbytes-dir reply --connbytes-mode bytes -j DSCP --set-dscp-class CS1


## some games use TCP, let's match on TCP streams using less than
## 150pps this probably is interactive rather than a bulk
## transfer.

ipt4dscp -p tcp -m set --match-set "realtimeset4" src  -m hashlimit --hashlimit-mode srcip,srcport,dstip,dstport --hashlimit-name tcphighprio4 --hashlimit-upto 150/second --hashlimit-burst 150 --hashlimit-rate-match --hashlimit-rate-interval 1 -j DSCP --set-dscp-class CS4

ipt4dscp -p tcp -m set --match-set "realtimeset4" dst  -m hashlimit --hashlimit-mode srcip,srcport,dstip,dstport --hashlimit-name tcphighprio4 --hashlimit-upto 150/second --hashlimit-burst 150 --hashlimit-rate-match --hashlimit-rate-interval 1 -j DSCP --set-dscp-class CS4

ipt6dscp -p tcp -m set --match-set "realtimeset6" src  -m hashlimit --hashlimit-mode srcip,srcport,dstip,dstport --hashlimit-name tcphighprio6 --hashlimit-upto 150/second --hashlimit-burst 150 --hashlimit-rate-match --hashlimit-rate-interval 1 -j DSCP --set-dscp-class CS4

ipt6dscp -p tcp -m set --match-set "realtimeset6" dst  -m hashlimit --hashlimit-mode srcip,srcport,dstip,dstport --hashlimit-name tcphighprio6 --hashlimit-upto 150/second --hashlimit-burst 150 --hashlimit-rate-match --hashlimit-rate-interval 1 -j DSCP --set-dscp-class CS4

