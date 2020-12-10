
# Router custom QoS script: SimpleHFSCgamerscript.sh

SimpleHFSCgamerscript.sh is a shell script that will set up highly
responsive QoS for a wired router with a single LAN on OpenWrt. It is
important that you use a single ethernet device for your LAN, not a
bridge with wifi, otherwise this script will not see all packets
coming down from the internet as it only regulates ONE lan device and
a bridge with ethernet and wifi contains two devices.

- log into your OpenWrt router
- cd /etc
- wget https://raw.githubusercontent.com/dlakelan/routerperf/master/SimpleHFSCgamerscript.sh
- chmod a+x /etc/SimpleHFSCgamerscript.sh
- wget https://raw.githubusercontent.com/dlakelan/routerperf/master/dscptag.sh
- edit SimpleHFSCgamerscript.sh to set your WAN and LAN interfaces, your network speeds, and the speed you reserve for your game (GAMEUP and GAMEDOWN)
- edit the script to include your gaming boxes in the set of ips that is prioritized for UDP: 

```
for ip4 in 192.168.1.111 192.168.1.222; do
    ipset add realtimeset4 "$ip4"
done

for ip6 in 2001:db8::1 2001:db8::2 ; do
    ipset add realtimeset6 "$ip6"
done
```
- edit the script to include bulk ports for torrent clients that you use
- edit dscptag.sh to add any rules you want to use for custom DSCP tagging

Now, run the script

./SimpleHFSCgamerscript.sh

or in your /etc/rc.local do

```
echo y | /etc/SimpleHFSCgamerscript.sh
```
so it starts at boot.

## What this script does:


This script sets up a HFSC queue system on your WAN and LAN ethernet
interfaces. It offers 5 classes of traffic. The most important class
is 1:11 which is for use by realtime UDP traffic to and from a list of
gaming machines which is set by you. Packets with DSCP tags CS5, CS6,
CS7 will be sent to the realtime queue. Later when QFQ is available on
OpenWrt we may enable sub-prioritizing these, such as making game
packets more important than in-game VOIP or things like that.

The remaining classes 1:12, 1:13, 1:14, 1:15 are non-realtime but have
different bandwidth and latency behavior when there is contention. You
can edit the dscptag.sh script to tag DSCP on whatever packets you
want to enter each class.

- CS4 goes to 1:12 which will have relatively low latency, good for interactive video chats, or casual gaming on a non-dedicated game machine
- CS3 or by default anything else goes to 1:13 which is for normal browsing
- CS2 goes to 1:14 which will tend to pause and allow other traffic to go ahead, this is useful for medium long downloads
- CS1 goes to 1:15 and has very poor bandwidth and long latency when there is contention with other classes. This is good for all-night torrenting etc.

All "normal" classes will use all available bandwidth if they are the
only class using bandwidth. The realtime class will only use at most
GAMEUP or GAMEDOWN.

In general for high speed connections the realtime bandwidth should be
around 10-15% of your bandwidth or less... But when your connections
are slow, we need them to be at least what the game actually needs. As
a guideline, Call Of Duty used about 160kbps upstream and 320kbps
downstream, so a good baseline is about double that each direction. By
default we do something smart but you can adjust the script if needed,
depending on the game you play.

This script will limit your download to at most 10x your upload, this
is to avoid flooding your upload with ACK packets that compete with
your gaming during large downloads by other devices. For slow speed
connections below 3Mbps, it also does MSS clamping to encourage your
TCP streams to use 540 byte packets to reduce the "lumpiness" of your
queue thereby reducing jitter and dropped packets.

This should allow you to game on a shared line down to in the range of
700kbps upstream, however of course having higher speed connections
will in general be better. A 3000kbps connection and above should have
absolutely fluid gaming traffic with proper tuning of the
settings. Testers have successfully played with fluid gaming on
16000kbps down / 830kbps up DSL lines.


# Router performance analysis scripts (other scripts in this github)

This software collects data on a router running OpenWRT (or other
linux) and creates a data file. It also includes scripts to transform
that data file into a file containing a single JSON array, and some
data analysis scripts in R.

The idea is we will eventually crowdsource a bunch of the data files,
and create predictions for reliable SQM shaping bandwidth that each
router supported by OpenWRT can handle. This portion of the project is
no longer active.


