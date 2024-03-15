#!/bin/sh

echo "Starting the uninstallation process..."

echo "Removing specific scripts..."

# Remove the specific script and its configuration
rm -f /etc/SimpleHFSCgamerscript.sh
rm -f /etc/hotplug.d/iface/13-SimpleHFSCGamerScriptHotplug
rm -f /usr/share/nftables.d/ruleset-post/dscptag.nft

echo "Removing veth interface configuration..."

# Remove the veth interface configuration
uci delete network.veth
uci commit network

echo "Checking for LAN firewall zone to remove veth interface..."

LAN_ZONE_NAME=$(uci show firewall | grep -E "firewall.@zone\[.*\].name='lan'" | cut -d'.' -f2)
if [ ! -z "$LAN_ZONE_NAME" ]; then
    uci del_list firewall.$LAN_ZONE_NAME.network='veth'
    uci commit firewall
    /etc/init.d/firewall reload
    echo "veth interface removed from LAN firewall zone."
else
    echo "LAN firewall zone does not exist, skipping removal."
fi

echo "Reloading network service..."
/etc/init.d/network reload

echo "Uninstallation completed successfully!"
