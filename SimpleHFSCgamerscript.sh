#!/bin/sh

## "atm" for old-school DSL or change to "DOCSIS" for cable modem, or
## "other" or anything else, for everything else

LINKTYPE="ethernet"

WAN=veth0 # change this to your WAN device name
UPRATE=18000 #change this to your kbps upload speed
LAN=veth1
DOWNRATE=65000 #change this to about 80% of your download speed (in kbps)
OH=40 # number of bytes of Overhead on your line (37 is reasonable
      # starting point, better to be too big than too small) probably
      # likely values are between 20 and 50

PFIFOMIN=5 ## minimum number of packets in pfifo, 4 to 10 is good guess
PACKETSIZE=350 # bytes per game packet avg (guess, 250 to 500 is likely) 
MAXDEL=25 # ms we try to keep max delay below for game packets after
	  # burst 10-25 is good 1 clock tick at 64Hz is ~16ms

BWMAXRATIO=20 ## prevent ack floods by limiting download to at most
	      ## upload times this amount... ratio somewhere between
	      ## 10 and 20 probably optimal. we down-prioritize
	      ## certain ACKs to reduce the chance of a flood as well.

if [ $((DOWNRATE > UPRATE*BWMAXRATIO)) -eq 1 ]; then
    echo "We limit the downrate to at most $BWMAXRATIO times the upstream rate to ensure no upstream ACK floods occur which can cause game packet drops"
    DOWNRATE=$((BWMAXRATIO*UPRATE))
fi

## how many kbps of UDP upload and download do you need for your games
## across all gaming machines? 

## you can tune these yourself, but a good starting point is this
## formula.  this script will not work for UPRATE less than about
## 600kbps or downrate less than about 1000kbps

GAMEUP=$((UPRATE*15/100+400))
GAMEDOWN=$((DOWNRATE*15/100+400))

## you can try setting GAMEUP and GAMEDOWN manually, some report this
## works well for CoD
#GAMEUP=400
#GAMEDOWN=800


DSCPSCRIPT="/etc/dscptag.sh"

if [ ! -f $DSCPSCRIPT ]; then
    workdir=$(pwd)
    echo "You do not have the DSCP tagging script, downloading from github"
    cd /etc/
    wget https://raw.githubusercontent.com/dlakelan/routerperf/master/dscptag.sh
    cd $workdir
fi



## Right now there are four possible leaf qdiscs: pfifo, red,
## fq_codel, or netem. If you use netem it's so you can intentionally
## add delay to your packets, set netemdelayms to the number of ms you
## want to add each direction. Our default is pfifo it is reported to
## be the best for use in the realtime queue

gameqdisc="pfifo"

#gameqdisc="netem"

netemdelayms="1"
netemjitterms="7"
netemdist="normal"

pktlossp="none" # set to "none" for no packet loss, or use a fraction
		# like 0.015 for 1.5% packet loss in the realtime UDP
		# streams


if [ $gameqdisc != "fq_codel" -a $gameqdisc != "red" -a $gameqdisc != "pfifo" -a $gameqdisc != "netem" ]; then
    echo "Other qdiscs are not tested and do not work on OpenWrt yet anyway, reverting to red"
    gameqdisc="red"
fi

## set up your ipsets here:

## get rid of any references to the ipsets
iptables -t mangle -F dscptag > /dev/null 2>&1


for set in realtimeset4 lowprioset4  ; do
    ipset destroy $set > /dev/null 2>&1
    ipset create $set hash:ip > /dev/null 2>&1
    ipset flush $set > /dev/null 2>&1
done

for set in realtimeset6 lowprioset6  ; do
    ipset destroy $set > /dev/null 2>&1
    ipset create $set hash:ip family inet6 > /dev/null 2>&1
    ipset flush $set > /dev/null 2>&1
done

## some examples to add your gaming devices to the realtime sets,
## allows you to have more than one console etc. Just add your ips
## into the list of ips in the for loop

for ip4 in 192.168.1.111 192.168.1.222; do
    ipset add realtimeset4 "$ip4"
done

for ip6 in 2001:db8::1 2001:db8::2 ; do
    ipset add realtimeset6 "$ip6"
done


### add ips of "low priority" machines, examples might include things
### that interfere with more important stuff, like gaming ;-). For
### example 4k TVs will typically buffer big chunks of data which can
### cause gaming stuttering but because they have buffers they don't
### really need super high priority



for ip4 in 192.168.1.111 192.168.1.222; do
    ipset add lowprioset4 "$ip4"
done

for ip6 in 2001:db8::1 2001:db8::2 ; do
    ipset add lowprioset6 "$ip6"
done




## Help the system prioritize your gaming by telling it what is bulk
## traffic ... define a list of udp and tcp ports used for bulk
## traffic such as torrents. By default we include the transmission
## torrent client default port 51413 and the default TCP ports for
## bittorrent. Use comma separated values or ranges A:B as shown. Set
## your torrent client to use a known port and include it here

UDPBULKPT="51413"
TCPBULKPT="51413,6881:6889"


WASHDSCPUP="yes"
WASHDSCPDOWN="yes"


######################### CUSTOMIZATIONS GO ABOVE THIS LINE ###########

cat <<EOF

This script prioritizes the UDP packets from / to a set of gaming
machines into a real-time HFSC queue with guaranteed total bandwidth 

Based on your settings:

Game upload guarantee = $GAMEUP kbps
Game download guarantee = $GAMEDOWN kbps

Download direction only works if you install this on a *wired* router
and there is a separate AP wired into your network, because otherwise
there are multiple parallel queues for traffic to leave your router
heading to the LAN.

Based on your link total bandwidth, the **minimum** amount of jitter
you should expect in your network is about:

UP = $(((1500*8)*3/UPRATE)) ms

DOWN = $(((1500*8)*3/DOWNRATE)) ms

In order to get lower minimum jitter you must upgrade the speed of
your link, no queuing system can help.

Please note for your display rate that:

at 30Hz, one on screen frame lasts:   33.3 ms
at 60Hz, one on screen frame lasts:   16.6 ms
at 144Hz, one on screen frame lasts:   6.9 ms

This means the typical gamer is sensitive to as little as on the order
of 5ms of jitter. To get 5ms minimum jitter you should have bandwidth
in each direction of at least:

$((1500*8*3/5)) kbps

The queue system can ONLY control bandwidth and jitter in the link
between your router and the VERY FIRST device in the ISP
network. Typically you will have 5 to 10 devices between your router
and your gaming server, any of those can have variable delay and ruin
your gaming, and there is NOTHING that your router can do about it.

EOF

ipt64 (){
    iptables $*
    ip6tables $*
}


setqdisc () {
DEV=$1
RATE=$2
MTU=1500
highrate=$((RATE*90/100))
lowrate=$((RATE*10/100))
gamerate=$3
useqdisc=$4
DIR=$5


tc qdisc del dev "$DEV" root > /dev/null 2>&1

case $LINKTYPE in
    "atm")
	tc qdisc replace dev "$DEV" handle 1: root stab mtu 2047 tsize 512 mpu 68 overhead ${OH} linklayer atm hfsc default 13
	;;
    "DOCSIS")
	tc qdisc replace dev $DEV stab overhead 25 linklayer ethernet handle 1: root hfsc default 13
	;;
    *)
	tc qdisc replace dev $DEV stab overhead 40 linklayer ethernet handle 1: root hfsc default 13
	;;
esac
     

DUR=$((5*1500*8/RATE))
if [ $DUR -lt 25 ]; then
    DUR=25
fi

# if we're on the LAN side, create a queue just for traffic from the
# router, like LUCI and DNS lookups
if [ $DIR = "lan" ]; then
    tc class add dev "$DEV" parent 1: classid 1:2 hfsc ls m1 50000kbit d "${DUR}ms" m2 10000kbit
fi


#limit the link overall:
tc class add dev "$DEV" parent 1: classid 1:1 hfsc ls m2 "${RATE}kbit" ul m2 "${RATE}kbit"


# high prio realtime class
tc class add dev "$DEV" parent 1:1 classid 1:11 hfsc rt m1 "$((RATE*97/100))kbit" d "${DUR}ms" m2 "${gamerate}kbit"

# fast non-realtime
tc class add dev "$DEV" parent 1:1 classid 1:12 hfsc ls m1 "$((RATE*70/100))kbit" d "${DUR}ms" m2 "$((RATE*30/100))kbit"

# normal
tc class add dev "$DEV" parent 1:1 classid 1:13 hfsc ls m1 "$((RATE*20/100))kbit" d "${DUR}ms" m2 "$((RATE*45/100))kbit"

# low prio
tc class add dev "$DEV" parent 1:1 classid 1:14 hfsc ls m1 "$((RATE*7/100))kbit" d "${DUR}ms" m2 "$((RATE*15/100))kbit"

# bulk
tc class add dev "$DEV" parent 1:1 classid 1:15 hfsc ls m1 "$((RATE*3/100))kbit" d "${DUR}ms" m2 "$((RATE*10/100))kbit"



## set this to "drr" or "qfq" to differentiate between different game
## packets, or use "pfifo" to treat all game packets equally

## games often use a 1/64 s = 15.6ms tick rate +- if we're getting so
## many packets that it takes that long to drain at full RATE, we're
## in trouble, because then everything lags by a full tick... so we
## set our RED minimum to start dropping at 9ms of packets at full
## line rate, and then drop 100% by 3x that much, it's better to drop
## packets for a little while than play a whole game lagged by a full
## tick

REDMIN=$((RATE*MAXDEL/3/8)) 

REDMAX=$((RATE * MAXDEL/8)) 

# for fq_codel
INTVL=$((100+2*1500*8/RATE))
TARG=$((540*8/RATE+4))



case $useqdisc in
    "drr")
	tc qdisc add dev "$DEV" parent 1:11 handle 2:0 drr
	tc class add dev "$DEV" parent 2:0 classid 2:1 drr quantum 8000
	tc qdisc add dev "$DEV" parent 2:1 handle 10: red limit 150000 min $REDMIN max $REDMAX avpkt 500 bandwidth ${RATE}kbit probability 1.0
	tc class add dev "$DEV" parent 2:0 classid 2:2 drr quantum 4000
	tc qdisc add dev "$DEV" parent 2:2 handle 20: red limit 150000 min $REDMIN max $REDMAX avpkt 500 bandwidth ${RATE}kbit probability 1.0
	tc class add dev "$DEV" parent 2:0 classid 2:3 drr quantum 1000
	tc qdisc add dev "$DEV" parent 2:3 handle 30: red limit 150000  min $REDMIN max $REDMAX avpkt 500 bandwidth ${RATE}kbit probability 1.0
	## with this send high priority game packets to 10:, medium to 20:, normal to 30:
	## games will not starve but be given relative importance based on the quantum parameter
    ;;

    "qfq")
	tc qdisc add dev "$DEV" parent 1:11 handle 2:0 qfq
	tc class add dev "$DEV" parent 2:0 classid 2:1 qfq weight 8000
	tc qdisc add dev "$DEV" parent 2:1 handle 10: red limit 150000  min $REDMIN max $REDMAX avpkt 500 bandwidth ${RATE}kbit probability 1.0
	tc class add dev "$DEV" parent 2:0 classid 2:2 qfq weight 4000
	tc qdisc add dev "$DEV" parent 2:2 handle 20: red limit 150000 min $REDMIN max $REDMAX avpkt 500 bandwidth ${RATE}kbit probability 1.0
	tc class add dev "$DEV" parent 2:0 classid 2:3 qfq weight 1000
	tc qdisc add dev "$DEV" parent 2:3 handle 30: red limit 150000  min $REDMIN max $REDMAX avpkt 500 bandwidth ${RATE}kbit probability 1.0
	## with this send high priority game packets to 10:, medium to 20:, normal to 30:
	## games will not starve but be given relative importance based on the weight parameter

    ;;

    "pfifo")
	tc qdisc add dev "$DEV" parent 1:11 handle 10: pfifo limit $((PFIFOMIN+MAXDEL*RATE/8/PACKETSIZE))
	;;
    "red")
	tc qdisc add dev "$DEV" parent 1:11 handle 10: red limit 150000 min $REDMIN max $REDMAX avpkt 500 bandwidth ${RATE}kbit  probability 1.0
	## send game packets to 10:, they're all treated the same
	;;
    "fq_codel")
	tc qdisc add dev "$DEV" parent "1:11" fq_codel memory_limit $((RATE*200/8)) interval "${INTVL}ms" target "${TARG}ms" quantum $((MTU * 2))
	;;
    "netem")
	tc qdisc add dev "$DEV" parent 1:11 handle 10: netem limit $((4+9*RATE/8/500)) delay "${netemdelayms}ms" "${netemjitterms}ms" distribution "$netemdist"
	;;


esac


echo "adding fq_codel qdisc for non-game traffic"
for i in 12 13 14 15; do 
    tc qdisc add dev "$DEV" parent "1:$i" fq_codel memory_limit $((RATE*200/8)) interval "${INTVL}ms" target "${TARG}ms" quantum $((MTU * 2))
done


}


setqdisc $WAN $UPRATE $GAMEUP $gameqdisc wan

## uncomment this to do the download direction via output of LAN
setqdisc $LAN $DOWNRATE $GAMEDOWN $gameqdisc lan

## we want to classify packets, so use these rules

cat <<EOF

We are going to add classification rules via iptables to the
FORWARD chain. You should actually read and ensure that these
rules make sense in your firewall before running this script. 

Continue? (type y or n and then RETURN/ENTER)
EOF

read -r cont

if [ "$cont" = "y" ]; then

    ipt64 -t mangle -F FORWARD

    ipt64 -t mangle -N dscptag
    ipt64 -t mangle -F dscptag
    
    
    if [ "$WASHDSCPUP" = "yes" ]; then
	ipt64 -t mangle -A FORWARD -i $LAN -j DSCP --set-dscp-class CS0
    fi
    if [ "$WASHDSCPDOWN" = "yes" ]; then
	ipt64 -t mangle -A FORWARD -i $WAN -j DSCP --set-dscp-class CS0
    fi

    ipt64 -t mangle -A POSTROUTING -j dscptag
    source $DSCPSCRIPT
    
    ipt64 -t mangle -A FORWARD -j CLASSIFY --set-class 1:13 # default everything to 1:13,  the "normal" qdisc

    # traffic from the router to the LAN bypasses the download queue
    ipt64 -t mangle -A OUTPUT -o $LAN -j CLASSIFY --set-class 1:2
    
    ## these dscp values go to realtime: EF, CS5, CS6, CS7
    ipt64 -t mangle -A POSTROUTING -m dscp --dscp-class EF -j CLASSIFY --set-class 1:11
    ipt64 -t mangle -A POSTROUTING -m dscp --dscp-class CS5 -j CLASSIFY --set-class 1:11
    ipt64 -t mangle -A POSTROUTING -m dscp --dscp-class CS6 -j CLASSIFY --set-class 1:11
    ipt64 -t mangle -A POSTROUTING -m dscp --dscp-class CS7 -j CLASSIFY --set-class 1:11
    
    ipt64 -t mangle -A POSTROUTING -m dscp --dscp-class CS4 -j CLASSIFY --set-class 1:12
    ipt64 -t mangle -A POSTROUTING -m dscp --dscp-class AF41 -j CLASSIFY --set-class 1:12
    ipt64 -t mangle -A POSTROUTING -m dscp --dscp-class AF42 -j CLASSIFY --set-class 1:12
    
    ipt64 -t mangle -A POSTROUTING -m dscp --dscp-class CS2 -j CLASSIFY --set-class 1:14
    ipt64 -t mangle -A POSTROUTING -m dscp --dscp-class CS1 -j CLASSIFY --set-class 1:15

    ## wash DSCP out to the ISP now that we used it for classifying

    if [ "$WASHDSCPUP" = "yes" ]; then
	ipt64 -t mangle -A FORWARD -o $WAN -j DSCP --set-dscp-class CS0
    fi

    
    case $gameqdisc in
	"red")
	;;
	"pfifo")
	;;
	*)
	    echo "YOU MUST PLACE CLASSIFIERS FOR YOUR GAME TRAFFIC HERE"
	    echo "SEND GAME TRAFFIC TO 2:1 (high) or 2:2 (medium) or 2:3 (normal)"
	    echo "Requires use of tc filters! -j CLASSIFY won't work!"
	    ;;
    esac
    
    
    if [ $UPRATE -lt 3000 ]; then
	ipt64 -t mangle -A FORWARD -p tcp --tcp-flags SYN,RST SYN -o $LAN -j TCPMSS --set-mss 540
    fi
    if [ $DOWNRATE -lt 3000 ]; then
	## need to clamp MSS to 540 bytes in both directions to reduce
	## the latency increase caused by 1 packet ahead of us in the
	## queue since rates are too low to send 1500 byte packets at acceptable delay
	ipt64 -t mangle -A FORWARD -p tcp --tcp-flags SYN,RST SYN -o $WAN -j TCPMSS --set-mss 540
    fi


else
    cat <<EOF
Check the rules and come back when you're ready.
EOF
fi

echo "DONE!"


if [ "$gameqdisc" = "red" ]; then
   echo "Can not output tc -s qdisc because it crashes on OpenWrt when using RED qdisc, but things are working!"
else
   tc -s qdisc
fi


