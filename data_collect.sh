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

echo "Data collection done. See data file in /tmp/stat_output.txt"

cat /proc/cpuinfo > /tmp/cpu_info.txt

## run analysis script here

exit 0
