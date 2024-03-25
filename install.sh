#!/bin/sh

echo "Starting the installation process..."

# Checking and installing required packages
REQUIRED_PACKAGES="kmod-sched ip-full kmod-veth tc"
for pkg in $REQUIRED_PACKAGES; do
    if ! opkg list-installed | grep -q "^$pkg "; then
        echo "Installing $pkg..."
        opkg update && opkg install "$pkg"
    fi
done

echo "Downloading and setting up scripts..."

# Download and set execution permissions for the scripts
wget -O /etc/SimpleHFSCgamerscript.sh https://raw.githubusercontent.com/dlakelan/routerperf/master/SimpleHFSCgamerscript.sh && chmod a+x /etc/SimpleHFSCgamerscript.sh
wget -O /etc/hotplug.d/iface/13-SimpleHFSCGamerScriptHotplug https://raw.githubusercontent.com/dlakelan/routerperf/master/13-SimpleHFSCGamerScriptHotplug
mkdir -p /usr/share/nftables.d/ruleset-post/ && wget -O /usr/share/nftables.d/ruleset-post/dscptag.nft https://raw.githubusercontent.com/dlakelan/routerperf/master/dscptag.nft

echo "Configuring veth interface..."

# Configure the veth interface
uci set network.veth=interface
uci set network.veth.proto='none'
uci set network.veth.ifname='lanveth'
uci set network.veth.device='lanveth'
uci commit network

echo "Checking for LAN firewall zone and adding veth interface if it exists..."

LAN_ZONE_NAME=$(uci show firewall | grep -E "firewall.@zone\[.*\].name='lan'" | cut -d'.' -f2)
if [ ! -z "$LAN_ZONE_NAME" ]; then
    uci add_list firewall.$LAN_ZONE_NAME.network='veth'
    uci commit firewall
    /etc/init.d/firewall reload
    echo "veth interface added to LAN firewall zone."
else
    echo "LAN firewall zone does not exist, skipping addition."
fi

echo "Reloading network service..."
/etc/init.d/network reload

echo "Installation completed successfully!"
