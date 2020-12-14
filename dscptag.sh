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


## boost DNS traffic
ipt4dscp -p udp --dport 53 -j DSCP --set-dscp-class CS4
ipt4dscp -p udp --sport 53 -j DSCP --set-dscp-class CS4


## boost the gaming machines UDP always to CS7 for realtime access
ipt4dscp -p udp -m set --match-set "${GAMINGIPSET4}" src -j DSCP --set-dscp-class CS7
ipt4dscp -p udp -m set --match-set "${GAMINGIPSET4}" dst -j DSCP --set-dscp-class CS7

ipt6dscp -p udp -m set --match-set "${GAMINGIPSET6}" src -j DSCP --set-dscp-class CS7
ipt6dscp -p udp -m set --match-set "${GAMINGIPSET6}" dst -j DSCP --set-dscp-class CS7


## If you game on a line with not quite enough bandwidth to accomodate
## both gaming and in-game VOIP, and you use a PS4, you can downgrade
## the in-game VOIP to get better hitreg etc: to downgrade VOIP from
## the gaming machine to high priority non-realtime uncomment this:

#ipt4dscp -p udp ! --sport 3074 -m set --match-set "${GAMINGIPSET4}" src -j DSCP --set-dscp-class CS4

## downgrade UDP tagged CS7 that sends more than 150 pps (seems
## unlikely to be gaming traffic, more likely QUIC), comment this out
## if you want, or change to CS1 to further down-priority

ipt64dscp -p udp -m DSCP --dscp-class CS7 -m hashlimit --hashlimit-mode srcip,srcport,dstip,dstport --hashlimit-above 150/second --hashlimit-rate-interval 1 -j DSCP --set-dscp-class CS2

## some games use TCP, let's match on TCP streams using less than
## 150pps this probably is interactive rather than a bulk
## transfer. This may ruin your game play for a few seconds if it's
## really the start of a big download. Only turn this on if you play a
## game that you know uses TCP, such as apparently FIFA

#ipt4dscp -p tcp -m set --match-set "${GAMINGIPSET4}" src  -m hashlimit --hashlimit-mode srcip,srcport,dstip,dstport --hashlimit-below 150/second --hashlimit-rate-interval 1 -j DSCP --set-dscp-class CS7

#ipt4dscp -p tcp -m set --match-set "${GAMINGIPSET4}" dst  -m hashlimit --hashlimit-mode srcip,srcport,dstip,dstport --hashlimit-below 150/second --hashlimit-rate-interval 1 -j DSCP --set-dscp-class CS7

#ipt6dscp -p tcp -m set --match-set "${GAMINGIPSET6}" src  -m hashlimit --hashlimit-mode srcip,srcport,dstip,dstport --hashlimit-below 150/second --hashlimit-rate-interval 1 -j DSCP --set-dscp-class CS7

#ipt6dscp -p tcp -m set --match-set "${GAMINGIPSET6}" dst  -m hashlimit --hashlimit-mode srcip,srcport,dstip,dstport --hashlimit-below 150/second --hashlimit-rate-interval 1 -j DSCP --set-dscp-class CS7
