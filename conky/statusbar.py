#!/usr/bin/python
import subprocess
import tempfile
import os
import signal
import argparse

## Handle the arguments
parser = argparse.ArgumentParser(description='Create a conky config')
parser.add_argument('--color-fg', default="#E8E8E8", help="The hex value of a color to use (default: #E8E8E8)", metavar="#000000")
parser.add_argument('--filename', help="Instead of outputting to stdout, store in the named file", metavar="FILE")
args = vars(parser.parse_args())

## Setup cleanup
def cleanup():
    os.remove(conkyFile)

def handleSigTERM():
    cleanup()

## Variables
colorschemeFgHex = "\\{}".format(args["color_fg"])
imagesDir = "$HOME/.conky/imgs"
if args["filename"]:
    conkyFile = args["filename"]
else:
    conkyFile = tempfile.NamedTemporaryFile('w', delete = False).name
    signal.signal(signal.SIGTERM, handleSigTERM)

f = open(conkyFile, 'w')

## CONKY SETTINGS
f.write("background yes\n")
f.write("out_to_console yes\n")
f.write("out_to_x no\n")
f.write("# Update interval in seconds\n")
f.write("update_interval 1\n")
f.write("\nTEXT\n")

## NETWORK
# Retrieve interfaces
tempFile = tempfile.NamedTemporaryFile()
os.system("ip link show up | egrep '^[0-9]*:' | awk -F: '$2 !~ /lo/ {{ sub(\"^ *\", \"\", $2); print $2 }}' > {}".format(tempFile.name))
interfaces = [line.strip() for line in tempFile]
tempFile.close()

# For each interface, generate conky output
# for interface in interfaces:
for interface in [intf.decode("utf-8") for intf in interfaces]:
    f.write("${{if_up {}}}\\\n".format(interface))
    if interface[0] == "w":
        f.write("${{wireless_essid {}}} \\\n".format(interface))
        f.write("^fg({})\\\n".format(colorschemeFgHex))
        f.write("${{if_match ${{wireless_link_qual_perc {}}} >= 95}}^i({}/wifi_100.xbm)${{else}}\\\n".format(interface, imagesDir))
        f.write("${{if_match ${{wireless_link_qual_perc {}}} >= 75}}^i({}/wifi_75.xbm)${{else}}\\\n".format(interface, imagesDir))
        f.write("${{if_match ${{wireless_link_qual_perc {}}} >= 50}}^i({}/wifi_50.xbm)${{else}}\\\n".format(interface, imagesDir))
        f.write("^i({}/wifi_25.xbm)\\\n".format(imagesDir))
        f.write("${endif}${endif}${endif} \\\n")
        # f.write("^fg(\\#FFFFFF) \\\n")
    # elif interface[0] == "e":
        #TODO: f.write("^i({}/ethernet.xbm)\\\n".format(imagesDir))
        #TODO: f.write("^fg(\\#006400)^i(DOWN) ${{downspeed {}}}^fg(\\#FFFFFF) \\\n".format(interface))
        #TODO: f.write("^fg(\\#FF0000)^i(UP) ${{upspeed {}}}^fg(\\#FFFFFF) \\\n".format(interface))
    f.write("^fg(\\#606060)${{addr {}}}^fg(\\#FFFFFF) \\\n".format(interface))
    f.write("${endif}\\\n")

# Lastly, output the external IP
f.write("^fg(\\#606060)(${exec wget -q -O /dev/stdout http://checkip.dyndns.org/ | cut -d : -f 2- | cut -d \\< -f -1 | awk '{ print $1 }'})^fg(\\#FFFFFF)")
f.write("    \\\n")

## CPU & RAM
# CPU
f.write("^fg(\\#606060)^i({}/cpu.xbm)^fg(\\#FFFFFF) \\\n".format(imagesDir))
numCpus = subprocess.check_output("grep -c 'processor' /proc/cpuinfo", shell=True).strip().decode("utf-8")
for cpu in range(1,int(numCpus)+1):
    f.write("${{if_match ${{cpu cpu{}}} >= 85}}^fg(\\#FF0000)${{endif}}\\\n".format(cpu))
    f.write("${{if_match ${{cpu cpu{}}} < 10}} ${{endif}}${{cpu cpu{}}}%^fg(\\#FFFFFF) \\\n".format(cpu, cpu))
# RAM
f.write("^fg(\\#737373)^i({}/mem.xbm)^fg(\\#FFFFFF) \\\n".format(imagesDir))
f.write("${if_match ${memperc} >= 85}^fg(\\#FF0000)${endif}${memperc}%   \\\n")

## MEDIA
f.write("^fg({})\\\n".format(colorschemeFgHex))
f.write("${if_match ${texeci 1 amixer get Master | grep Mono: | grep -c '\[off\]'} >= 1}\\\n")
f.write("^i({}/spkr_02.xbm)\\\n".format(imagesDir))
f.write("^fg(\#3A3A3A) \\\n")
f.write("${else}\\\n")
f.write("^i({}/spkr_01.xbm)\\\n".format(imagesDir))
f.write("^fg(\#FFFFFF) \\\n")
f.write("${endif}\\\n")
f.write("${texeci 1 amixer get Master | tail -1 | cut -d'[' -f2 | cut -d']' -f1}  \\\n")
f.write("^fg(\#FFFFFF) \\\n")

## TIME
f.write("${{time %a}}, ^fg({})${{time %d}}^fg(\#FFFFFF) ^fg(\#737373)${{time %b}}^fg(\#FFFFFF) \\\n".format(colorschemeFgHex))
f.write("^fg({})${{time %H%M}}^fg(\#FFFFFF) \\\n".format(colorschemeFgHex))
f.write("^fg(\#3A3A3A)(${uptime})^fg(\#FFFFFF)  \\\n")

## BATTERY
f.write("^fg({})\\\n".format(colorschemeFgHex))
f.write("${if_match ${battery_percent} < 50}^fg(\\#FFCC00)${endif}\\\n")
f.write("${if_match ${battery_percent} < 20}^fg(\\#FF0000)${endif}\\\n")
batterySteps = [100, 94, 88, 82, 75, 69, 63, 56, 50, 44, 38, 31, 25, 19, 12, 6]
for i in batterySteps:
    f.write("${{if_match ${{battery_percent}} >= {}}}^i({}/battery_{}.xbm)${{else}}\\\n".format(i, imagesDir, i))
f.write("^i({}/battery_0.xbm)\\\n".format(imagesDir))
for i in range(len(batterySteps)):
    f.write("${endif}")
f.write("\n")
f.close()

# os.system("cp {} /tmp/statusbar.conkyrc".format(conkyFile))
if not args["filename"]:
    for out in open(conkyFile):
        print(out, end="")
    cleanup()
