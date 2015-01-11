#!/usr/bin/python
import subprocess
import tempfile
import os
import signal
import argparse
import sys

## Handle the arguments
f=open('/tmp/statusbar.py.out', 'w+')
f.write(" ".join(sys.argv))

parser = argparse.ArgumentParser(description='Create a conky config')
parser.add_argument('--color-fg',
                    default="#E8E8E8",
                    help="The hex value of a color to use (default: #E8E8E8)",
                    metavar="#000000")
parser.add_argument('--filename',
                    help="Instead of outputting to stdout, store in the named file",
                    metavar="FILE")
args = vars(parser.parse_args())

## Setup cleanup
def cleanup():
    os.remove(conkyFile)

def handleSigTERM():
    cleanup()

## Variables
colorschemeFgHex = "\\{}".format(args["color_fg"])
imagesDir = "$HOME/.conky/imgs"
colorschemeGreyHex = "\\#606060"
colorschemeDimHex = "\\#3A3A3A"
colorschemeDarkHex = "\\#282828"
colorschemeWhiteHex = "\\#FFFFFF"
colorschemeRedHex = "\\#FF0000"
colorschemeGreenHex = "\\#006400"
colorschemeYellowHex = "\\#FFCC00"
if args["filename"]:
    conkyFile = args["filename"]
else:
    conkyFile = tempfile.NamedTemporaryFile('w', delete = False).name
    signal.signal(signal.SIGTERM, handleSigTERM)
sectionSpacing = "     \\\n"

f = open(conkyFile, 'w')

## CONKY SETTINGS
f.write("background yes\n")
f.write("out_to_console yes\n")
f.write("out_to_x no\n")
f.write("# Update interval in seconds\n")
f.write("update_interval 1\n")
f.write("\nTEXT\n")

## Machine info
f.write("^fg({})${{nodename}}\\\n".format(colorschemeGreyHex))
# TODO: if arch linux:
f.write(" ^fg({})^i({}/arch.xbm) \\\n".format(colorschemeDimHex, imagesDir))
# else:
#   f.write(" ^fg({})| \\\n".format(colorschemeDarkHex))
f.write("^fg({})${{kernel}}\\\n".format(colorschemeGreyHex))
f.write(sectionSpacing)

## NETWORK
# Retrieve interfaces
tempFile = tempfile.NamedTemporaryFile()
os.system("ip link show up | egrep '^[0-9]*:' | awk -F: '$2 !~ /lo/ {{ sub(\"^ *\", \"\", $2); print $2 }}' > {}".format(tempFile.name))
interfaces = [line.strip() for line in tempFile]
tempFile.close()

# For each interface, generate conky output
# for interface in interfaces:
for interface in [intf.decode("utf-8") for intf in interfaces]:
    # f.write("${{if_up {}}}^fg({})\\\n".format(interface, colorschemeWhiteHex))
    f.write("${{if_up {}}}^fg({})\\\n".format(interface, colorschemeFgHex))
    if interface[0] == "w":
        # f.write("Steve Taylor's Guest Network \\\n")
        f.write("${{wireless_essid {}}} \\\n".format(interface))
        # f.write("^fg({})\\\n".format(colorschemeFgHex))
        f.write("^fg({})\\\n".format(colorschemeWhiteHex))
        f.write("${{if_match ${{wireless_link_qual_perc {}}} >= 95}}^i({}/wifi_100.xbm)${{else}}\\\n".format(interface, imagesDir))
        f.write("${{if_match ${{wireless_link_qual_perc {}}} >= 75}}^i({}/wifi_75.xbm)${{else}}\\\n".format(interface, imagesDir))
        f.write("${{if_match ${{wireless_link_qual_perc {}}} >= 50}}^i({}/wifi_50.xbm)${{else}}\\\n".format(interface, imagesDir))
        f.write("^i({}/wifi_25.xbm)\\\n".format(imagesDir))
        f.write("${endif}${endif}${endif} \\\n")
    elif interface[0] == "e":
        f.write("^fg({})\\\n".format(colorschemeFgHex))
        f.write("^i({}/ethernet.xbm)\\\n".format(imagesDir))
    f.write("^fg({})${{addr {}}}^fg({}) \\\n".format(colorschemeGreyHex, interface, colorschemeWhiteHex))
    f.write("${endif}\\\n")

# Lastly, output the external IP
f.write("^fg({})".format(colorschemeGreyHex))
f.write("(${exec wget -q -O /dev/stdout http://checkip.dyndns.org/ | cut -d : -f 2- | cut -d \\< -f -1 | awk '{ print $1 }'})")
f.write(" \\\n")
f.write("^fg({})${{if_match ${{downspeedf {}}} > 1.5}}^fg({})${{endif}}\\\n".format(colorschemeDimHex, interface, colorschemeFgHex))
f.write("^i({}/net_down.xbm)\\\n".format(imagesDir))
f.write("^fg({})${{if_match ${{upspeedf {}}} > 1.5}}^fg({})${{endif}}\\\n".format(colorschemeDimHex, interface, colorschemeFgHex))
f.write("^i({}/net_up.xbm)\\\n".format(imagesDir))
f.write(sectionSpacing)

## CPU & RAM
# CPU
f.write("^fg({})^i({}/cpu.xbm) \\\n".format(colorschemeGreyHex, imagesDir))
numCpus = subprocess.check_output("grep -c 'processor' /proc/cpuinfo", shell=True).strip().decode("utf-8")
for cpu in range(1,int(numCpus)+1):
    f.write("^fg({})\\\n".format(colorschemeWhiteHex))
    f.write("${{if_match ${{cpu cpu{}}} >= 85}}^fg({})${{endif}}\\\n".format(cpu, colorschemeRedHex))
    f.write("${{if_match ${{cpu cpu{}}} < 10}} ^fg({})${{endif}}\\\n".format(cpu, colorschemeDimHex))
    f.write("${{cpu cpu{}}}% \\\n".format(cpu))
# RAM
f.write(" ^fg({})^i({}/mem.xbm)^fg({}) \\\n".format(colorschemeGreyHex, imagesDir, colorschemeWhiteHex))
f.write("${{if_match ${{memperc}} < 50}}^fg({})${{endif}}\\\n".format(colorschemeDimHex))
f.write("${{if_match ${{memperc}} >= 85}}^fg({})${{endif}}\\\n".format(colorschemeRedHex))
f.write("${memperc}%")
f.write(sectionSpacing)
# TEMP
# tempVar = "${hwmon temp 0}"
# tempVar = "${acpitemp}"
# f.write("^fg({})^i({}/temp.xbm) \\\n".format(colorschemeGreyHex, imagesDir))
# f.write("^fg({})\\\n".format(colorschemeWhiteHex))
# f.write("${{if_match {} <= 60}}^fg({})${{endif}}\\\n".format(tempVar, colorschemeDimHex))
# f.write("${{if_match {} >= 80}}^fg({})${{endif}}\\\n".format(tempVar, colorschemeRedHex))
# f.write("{}°".format(tempVar))
# f.write(" \\\n")
# DISK
# f.write("${if_match ${fs_used_perc /} > 80}\\\n")
# f.write("${else}\\\n")
# f.write("${endif}\\\n")
# f.write("${if_match ${fs_used_perc /home} > 80}\\\n")
# f.write("${else}\\\n")
# f.write("${endif}\\\n")
# f.write(" ^fg({})^i({}/diskette.xbm) \\\n".format(colorschemeGreyHex, imagesDir))
# f.write("   \\\n")

## MEDIA
f.write("^fg({})\\\n".format(colorschemeFgHex))
# TODO: if headphones are plugged in, then use plug.xbm and the headphones level
f.write("${if_match ${texeci 1 amixer get Master | grep Mono: | grep -c '\[off\]'} >= 1}\\\n")
f.write("^i({}/spkr_02.xbm)".format(imagesDir))
f.write("^fg({})".format(colorschemeDimHex))
f.write("${else}\\\n")
f.write("^i({}/spkr_01.xbm)".format(imagesDir))
f.write("^fg({})".format(colorschemeGreyHex))
f.write("${endif}\\\n")
f.write(" ${texeci 1 amixer get Master | tail -1 | cut -d'[' -f2 | cut -d']' -f1}\\\n")
f.write("   \\\n")

## TIME
f.write("^fg({})".format(colorschemeGreyHex))
f.write("${{time %a}} ^fg({})${{time %d}} ^fg({})${{time %b}} \\\n".format(colorschemeFgHex, colorschemeDimHex))
f.write("^fg({})${{time %H%M}}^fg({}) \\\n".format(colorschemeWhiteHex, colorschemeWhiteHex))
f.write(" ^fg({})${{uptime}}^fg({})\\\n".format(colorschemeDimHex, colorschemeWhiteHex))
f.write("   \\\n")

## BATTERY
f.write("^fg({})\\\n".format(colorschemeDarkHex))
f.write("${{if_match ${{battery_percent}} < 99}}^fg({})${{endif}}\\\n".format(colorschemeFgHex))
f.write("${{if_match ${{battery_percent}} < 50}}^fg({})${{endif}}\\\n".format(colorschemeYellowHex))
f.write("${{if_match ${{battery_percent}} < 20}}^fg({})${{endif}}\\\n".format(colorschemeRedHex))
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

