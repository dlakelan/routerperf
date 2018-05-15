#!/bin/sh


i=1

echo "Beginning data collection. Run speed test now."

## Run background ping to 8.8.8.8
(
    echo "pingformat(1)" > /tmp/pings.txt
    date +"nstimestart(%s,%N)" >> /tmp/pings.txt ;
   ping -i 0.2 -c 300 -s 16 -D 8.8.8.8 >> /tmp/pings.txt 
) &

## overwrite the output file with format info and initial timestamp
echo "statformat(1)" > /tmp/stat_output.txt
date +"nstimestart(%s,%N)" >> /tmp/stat_output.txt

while [ "$i" -lt 600 ]; do ## 60 seconds of data collection
   date +"nstimestamp(%s,%N)" >> /tmp/stat_output.txt
   echo -n 'procstat(`' >> /tmp/stat_output.txt
   cat /proc/stat >> /tmp/stat_output.txt
   echo "')" >> /tmp/stat_output.txt
   echo -n 'procnetdev(`' >> /tmp/stat_output.txt
   cat /proc/net/dev >> /tmp/stat_output.txt
   echo "')" >> /tmp/stat_output.txt
   sleep 0.1
   i=$(( ${i} + 1 ))
done

## collect some basic info about the router
echo "" > /tmp/router_info.txt
echo 'CPUinfo(`' >> /tmp/router_info.txt
cat /proc/cpuinfo >> /tmp/router_info.txt
echo "')" >> /tmp/router_info.txt
echo 'cpuhzest(`' >> /tmp/router_info.txt
awk '{print "HZ="$22/'$(cat /proc/uptime | cut -d " " -f1)"}" /proc/self/stat >> /tmp/router_info.txt
echo "')" >> /tmp/router_info.txt


echo "Data collection done. See data file in /tmp/stat_output.txt and router info in /tmp/router_info.txt"

## potentially request permission and upload data here

exit 0
