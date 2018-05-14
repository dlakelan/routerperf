#!/bin/sh

## truncate the output file
echo "" > /tmp/stat_output.txt
i=1

echo "Beginning data collection. Run speed test now."

while [ "$i" -lt 600 ]; do ## 60 seconds of data collection
   echo "###date" >> /tmp/stat_output.txt
   date +%s%N >> /tmp/stat_output.txt
   echo "###/proc/stat" >> /tmp/stat_output.txt
   cat /proc/stat >> /tmp/stat_output.txt
   echo "" >> /tmp/stat_output.txt ## blank line for separating two files
   echo "###/proc/net/dev" >> /tmp/stat_output.txt
   cat /proc/net/dev >> /tmp/stat_output.txt
   sleep 0.1
   i=$(( ${i} + 1 ))
done


echo "" > /tmp/router_info.txt
echo "# CPUinfo" >> /tmp/router_info.txt
cat /proc/cpuinfo >> /tmp/router_info.txt
echo "# HZ estimate" >> /tmp/router_info.txt
awk '{print "HZ="$22/'$(cat /proc/uptime | cut -d " " -f1)"}" /proc/self/stat >> /tmp/router_info.txt


echo "Data collection done. See data file in /tmp/stat_output.txt and router info in /tmp/router_info.txt"

## potentially request permission and upload data here

exit 0
