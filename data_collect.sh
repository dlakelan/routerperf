#!/bin/sh

## START: user editable section:
TESTDURATION_SECS=60
PING_HOST_IP="8.8.8.8"
## END: user editable section:


## variable intialisation
required_gnu_coreutils="sleep date"
required_iputils="ping"
i=1
ping_pid=0



is_binary_from_coreutils() {
    local BINARY=${1}
    #echo "Testing whether ${BINARY} is the GNU coreutils version."
    local TMP=$( ${BINARY} --v 2>/dev/null | grep -e "GNU coreutils" )
    if [ -z "${TMP}" ] ; then
        echo "The ${BINARY} binary is not the required version from GNU coreutils."
        if [ -f "/etc/os-release" ] ; then
    	    local TMP2=$( cat /etc/os-release | grep -m 1 -o -e openwrt )
    	    if [ ! -z "${TMP2}" ] ; then
    		echo "on openwrt/lede you might want to try running: opkg update ; opkg install coreutils-${BINARY}"
    	    fi
        fi
        exit 0
    fi
}

is_binary_from_iputils() {
    local BINARY=${1}
    #echo "Testing whether ${BINARY} is the iputils version."
    local TMP=$( ${BINARY} -V 2>/dev/null | grep -e "iputils" )
    if [ -z "${TMP}" ] ; then
        echo "The ${BINARY} binary is not the required version from iputils."
        if [ -f "/etc/os-release" ] ; then
    	    local TMP2=$( cat /etc/os-release | grep -e openwrt )
    	    if [ -z "${TMP2}" ] ; then
    		echo "on openwrt/lede you might want to try running: opkg update ; opkg install iputils-${BINARY}"
    	    fi
        fi
        exit 0
    fi
}




# try to be helpful about 
test_required_binaries() {
    # GNU coreutils

    for current_binary in $( echo ${required_gnu_coreutils} ) ; do
	echo "Current binary: ${current_binary}"
	is_binary_from_coreutils ${current_binary} 
    done

    # iputils
    for current_binary in $( echo ${required_iputils} ) ; do
	echo "Current binary: ${current_binary}"
	is_binary_from_iputils ${current_binary} 
    done

}



## Instead of running a fixed number of pings run it for -c $BIGNUM but kill it after the data collection has finished
kill_pings() {
    if [ "${ping_pid}" -ne "0" ] ; then
	echo "Terminating running ICMP data collection"
	kill -9 $ping_pid 
	wait $ping_pid 2>/dev/null
	ping_pid=0
    else
	echo "No ping running, nothing to terminate..."
    fi
}


echo "Testing for required binaries..."
test_required_binaries


echo "Beginning data collection. Run speed test now."

## Run background ping to 8.8.8.8
echo "Starting timestamped ICMP collecton in the background."
echo "pingformat(1)" > /tmp/pings.txt
date +"nstimestart(%s,%N)" >> /tmp/pings.txt ;
ping -i 0.2 -c $(( ${TESTDURATION_SECS} * 5 * 10 )) -s 16 -D ${PING_HOST_IP} >> /tmp/pings.txt &
ping_pid=$!


## overwrite the output file with format info and initial timestamp
echo "statformat(1)" > /tmp/stat_output.txt
date +"nstimestart(%s,%N)" >> /tmp/stat_output.txt

while [ "$i" -lt $(( TESTDURATION_SECS * 10 )) ]; do ## 60 seconds of data collection
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


## clean up
kill_pings


## collect some basic info about the router
echo "" > /tmp/router_info.txt
echo 'CPUinfo(`' >> /tmp/router_info.txt
cat /proc/cpuinfo >> /tmp/router_info.txt
echo "')" >> /tmp/router_info.txt
echo 'cpuhzest(`' >> /tmp/router_info.txt
awk '{print "HZ="$22/'$(cat /proc/uptime | cut -d " " -f1)"}" /proc/self/stat >> /tmp/router_info.txt
echo "')" >> /tmp/router_info.txt


if [ -f "/etc/config/sqm" ] ; then
    echo "Found sqm-scripts config file; saving to router_info.txt."
    echo 'etc_config_sqm(`' >> /tmp/router_info.txt
    cat /etc/config/sqm >> /tmp/router_info.txt
    echo "')" >> /tmp/router_info.txt
fi

if [ -f "/etc/config/qos" ] ; then
    echo "Found qos-scropts config file; saving to router_info.txt."
    echo 'etc_config_qos(`' >> /tmp/router_info.txt
    cat /etc/config/qos >> /tmp/router_info.txt
    echo "')" >> /tmp/router_info.txt
fi



echo "Data collection done. See data file in /tmp/stat_output.txt and router info in /tmp/router_info.txt"

## now move all output file in to one compressed archive?


## potentially request permission and upload data here

exit 0
