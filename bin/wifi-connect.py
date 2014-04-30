#!/usr/bin/python

import subprocess

# iw dev ${intf} scan | egrep "[SSID|RSN|WPA]:"

# cat ./devs | egrep "(SSID|RSN|WPA|capability):" | \
    # sed -e 's/\(SSID:\s.*\)/\1/g' -e 's/^\s*\(RSN\):.*$/WPA2/' -e 's/^\s*\(WPA\):.*$/\1/' \
        # -e 's/\s*capability: .* Privacy.*$/WEP/' -e 's/\s*capability: .*$/NONE/'

# Retrieve current wifi hotspots
# output = subprocess.check_output("iw dev ??? scan...", shell=True)
output = open("/tmp/foundDevices")
foundDevices = dict([line.strip().split(' ') for line in output])

# Retrieve known wifi hotspots
knownDevices = dict([line.strip().split(',') for line in open('/tmp/knownDevices')])
# knownDevices = dict([line.strip().split(',') for line in open('/??/../??/known_devices')])

hits = set(knownDevices.keys()) & set(foundDevices.keys())

top = hits.pop()
security = foundDevices[top]
key = knownDevices[top]

print("found: ", foundDevices)
print("known: ", knownDevices)
print("hits:", hits)
print("Security:", security)
print("Key:", key)
