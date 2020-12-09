# Router performance analysis scripts

This software collects data on a router running OpenWRT (or other
linux) and creates a data file. It also includes scripts to transform
that data file into a file containing a single JSON array, and some
data analysis scripts in R.

The idea is we will eventually crowdsource a bunch of the data files,
and create predictions for reliable SQM shaping bandwidth that each
router supported by OpenWRT can handle.


# Router custom QoS script:

SimpleHFSCgamerscript.sh is a shell script that will set up highly
responsive QoS for a wired router with a single LAN on OpenWrt

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
so it starts at boot
