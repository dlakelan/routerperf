#!/bin/sh 

#TODO:
#	collect tc -s qdisc before and after the /proc/stat polling to get a better view into the shaper
#	introduce and parse command line arguments: "duration NN", "-4" or "-6", "LANif", "WANif"



## START: user editable section:
TESTDURATION_SECS=60
PING_HOST_IP4="8.8.8.8"
PING_HOST_IP6="2001:4860:4860::8888"
## END: user editable section:


## variable intialisation
required_gnu_coreutils="sleep date"
required_iputils="ping ping6"
i=1
ping_pid=0



is_binary_from_coreutils() {
    local BINARY=${1}
    #echo "Testing whether ${BINARY} is the GNU coreutils version."
    local TMP="$( ${BINARY} --v 2>/dev/null | grep -e "GNU coreutils" )"
    if [ -z "${TMP}" ] ; then
        echo "The ${BINARY} binary is not the required version from GNU coreutils."
        if [ -f "/etc/os-release" ] ; then
    	    local TMP2="$( cat /etc/os-release | grep -m 1 -o -e openwrt )"
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
    local TMP="$( ${BINARY} -V 2>/dev/null | grep -e "iputils" )"
    if [ -z "${TMP}" ] ; then
        echo "The ${BINARY} binary is not the required version from iputils."
        if [ -f "/etc/os-release" ] ; then
    	    local TMP2=$( cat /etc/os-release | grep -m 1 -o -e openwrt )
    	    if [ ! -z "${TMP2}" ] ; then
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
	#echo "Current binary: ${current_binary}"
	is_binary_from_coreutils ${current_binary} 
    done

    # iputils
    for current_binary in $( echo ${required_iputils} ) ; do
	#echo "Current binary: ${current_binary}"
	is_binary_from_iputils ${current_binary} 
    done

}



## Instead of running a fixed number of pings run it for -c $BIGNUM but kill it after the data collection has finished
kill_pings() {
    if [ "${ping_pid}" -ne "0" ] ; then
	# check that ping_pid points to a ping process
	local TMP="$( ps | grep -E "^\s*${ping_pid} " | grep "ping" )"
	
	if [ ! -z "${TMP}" ] ; then
	    echo "Terminating running ICMP data collection"
	    kill -9 $ping_pid 
	    wait $ping_pid 2>/dev/null
	    ping_pid=0
	    echo "')">>${PING_COLLECTION_FILE}
	else
	    echo "Expected ping pid (${ping_pid}) does not point to a running ping binary, nothing to terminate..."
	fi
    else
	echo "No ping running, nothing to terminate..."
    fi
}



# set an initial values for defaults
TESTPROTO="-4"
WANIF=""
LANIF=""

# read the options

# extract options and their arguments into variables.
USAGE_STRING="Usage: sh data_collect.sh -W WAN_interface_name -L LAN_interface_name [-4 -6] [ -d duration ] [ -p host-to-ping ]"
if [ $# -eq 0 ] ; then
    echo ${USAGE_STRING}
    exit 1
fi

while [ $# -gt 0 ] 
do
    case "$1" in
	-w|--wan|--wanif|--WAN|-W)
	    case "$2" in
        	"") echo "Missing WAN interface name" ; exit 1 ;;
                *) WANIF=$2 ; shift 2 ;;		
	    esac ;;
	-l|--lan|--lanif|--lAN|-L)
	    case "$2" in
        	"") echo "Missing LAN interface name" ; exit 1 ;;
                *) LANIF=$2 ; shift 2 ;;		
	    esac ;;
	-4|-6) TESTPROTO=$1 ; shift 1 ;;
	-t|--time|-d|--duration|-T|-D) 
	    case "$2" in
        	#"") echo "Missing duration" ; exit 1 ;;
                *) TESTDURATION_SECS=$2 ; shift 2 ;;
    	    esac ;;
        -p|--ping|--pinghost|-P)
            case "$2" in
                #"") echo "Missing ping host" ; exit 1 ;;
                *) PINGHOST=$2 ; shift 2 ;;
            esac ;;
        --) shift ; break ;;
        *) echo ${USAGE_STRING} ; exit 1 ;;
    esac
done

if [ ! -z "${PINHGHOST}" ] ; then
    if [ $TESTPROTO -eq "-4" ] ; then
	PING_HOST_IP4=${PINGHOST}
    else
	PING_HOST_IP6=${PINGHOST}
    fi
fi



echo "Testing for required binaries..."
test_required_binaries


# create the directory to store all the assorted data files
BASE_DIR="/tmp"
SESSION_DATETIME=$( date "+%Y%m%dT%H%M%S" )
SESSION_DIR=${BASE_DIR}/routerperf.data.${SESSION_DATETIME} 
mkdir -p ${SESSION_DIR}
PING_COLLECTION_FILE=${SESSION_DIR}/ping_output.txt
PROC_STAT_COLLECTION_FILE=${SESSION_DIR}/stat_output.txt
ROUTER_INFO_COLLECTION_FILE=${SESSION_DIR}/router_info.txt
TC_STAT_COLLECTION_FILE=${SESSION_DIR}/tc_stat_output.txt


## dump the tc qdisc statistics at the beginning
echo "" > ${TC_STAT_COLLECTION_FILE}
echo 'TCstatisticsStart(`' >> ${TC_STAT_COLLECTION_FILE}
tc -s qdisc >> ${TC_STAT_COLLECTION_FILE}
echo "')" >> ${TC_STAT_COLLECTION_FILE}


## get a consistent start time for the whole process
STARTTIME=$(date +"nstimestart(%s,%N)")


## Run background ping to ${PINGHOST}
echo "Starting timestamped ICMP collection in the background."
echo "pingformat(1)" > ${PING_COLLECTION_FILE}
echo $STARTTIME >> ${PING_COLLECTION_FILE} ;
echo 'pingdata(`' >> ${PING_COLLECTION_FILE};
# this will instruct ping to run 10 times longer than required, but after the main data collection is finished we will ternminate ping
# NOTE:	that on 64 bit platforms only ping sizes >= bytes get RTT values (on 32 platforms size must be >= 8)
# 	for simplicity always use size = 16...
if [ $TESTPROTO -eq "-4" ] ; then
    ping -i 0.2 -c $(( ${TESTDURATION_SECS} * 5 * 10 )) -s 16 -D ${PING_HOST_IP4} >> ${PING_COLLECTION_FILE} &
else
    ping6 -i 0.2 -c $(( ${TESTDURATION_SECS} * 5 * 10 )) -s 16 -D ${PING_HOST_IP6} >> ${PING_COLLECTION_FILE} &
fi
ping_pid=$!


echo "Beginning data collection (for ${TESTDURATION_SECS} seconds). Run speed test in 10 seconds."
## overwrite the output file with format info and initial timestamp
echo "statformat(1)" > ${PROC_STAT_COLLECTION_FILE}

echo $STARTTIME >> ${PROC_STAT_COLLECTION_FILE}

while [ "$i" -lt $(( TESTDURATION_SECS * 10 )) ]; do ## 60 seconds of data collection
   date +"nstimestamp(%s,%N)" >> ${PROC_STAT_COLLECTION_FILE}
   echo -n 'procstat(`' >> ${PROC_STAT_COLLECTION_FILE}
   cat /proc/stat >> ${PROC_STAT_COLLECTION_FILE}
   echo "')" >> ${PROC_STAT_COLLECTION_FILE}
   echo -n 'procnetdev(`' >> ${PROC_STAT_COLLECTION_FILE}
   cat /proc/net/dev >> ${PROC_STAT_COLLECTION_FILE}
   echo "')" >> ${PROC_STAT_COLLECTION_FILE}
   sleep 0.1
   i=$(( ${i} + 1 ))
done


## clean up
kill_pings


## collect some basic info about the router
echo "" > ${ROUTER_INFO_COLLECTION_FILE}
echo 'WANinterface(`' >> ${ROUTER_INFO_COLLECTION_FILE}
echo ${WANIF} >> ${ROUTER_INFO_COLLECTION_FILE}
echo "')" >> ${ROUTER_INFO_COLLECTION_FILE}
echo 'LANinterface(`' >> ${ROUTER_INFO_COLLECTION_FILE}
echo ${LANIF} >> ${ROUTER_INFO_COLLECTION_FILE}
echo "')" >> ${ROUTER_INFO_COLLECTION_FILE}


echo 'CPUinfo(`' >> ${ROUTER_INFO_COLLECTION_FILE}
cat /proc/cpuinfo >> ${ROUTER_INFO_COLLECTION_FILE}
echo "')" >> ${ROUTER_INFO_COLLECTION_FILE}
echo 'cpuhzest(`' >> ${ROUTER_INFO_COLLECTION_FILE}
awk '{print "HZ="$22/'$(cat /proc/uptime | cut -d " " -f1)"}" /proc/self/stat >> ${ROUTER_INFO_COLLECTION_FILE}
echo "')" >> ${ROUTER_INFO_COLLECTION_FILE}


if [ -f "/etc/config/sqm" ] ; then
    echo "Found sqm-scripts config file; saving to router_info.txt."
    echo 'etc_config_sqm(`' >> ${ROUTER_INFO_COLLECTION_FILE}
    cat /etc/config/sqm >> ${ROUTER_INFO_COLLECTION_FILE}
    echo "')" >> ${ROUTER_INFO_COLLECTION_FILE}
fi

if [ -f "/etc/config/qos" ] ; then
    echo "Found qos-scropts config file; saving to router_info.txt."
    echo 'etc_config_qos(`' >> ${ROUTER_INFO_COLLECTION_FILE}
    cat /etc/config/qos >> ${ROUTER_INFO_COLLECTION_FILE}
    echo "')" >> ${ROUTER_INFO_COLLECTION_FILE}
fi


## dump the tc qdisc statistics at the end
echo 'TCstatisticsEnd(`' >> ${TC_STAT_COLLECTION_FILE}
tc -s qdisc >> ${TC_STAT_COLLECTION_FILE}
echo "')" >> ${TC_STAT_COLLECTION_FILE}


echo "Data collection done. See data files:"
echo "/proc output: ${PROC_STAT_COLLECTION_FILE}"
echo "router info: ${ROUTER_INFO_COLLECTION_FILE}"
echo "ICMP data: ${PING_COLLECTION_FILE}"
echo "Please check all data files for sensitive private information before uploading to the internet."

## now move all output file in to one compressed archive?


## potentially request permission and upload data here

exit 0
