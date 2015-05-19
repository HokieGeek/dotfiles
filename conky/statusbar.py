#!/usr/bin/python
import subprocess
import tempfile
import os
import signal
import sys

## Handle the arguments
f = open('/tmp/statusbar.py.out', 'w+')
f.write(" ".join(sys.argv))

## Setup cleanup
def cleanup():
    os.remove(conkyFile)

def handleSigTERM():
    cleanup()

## Variables
infoFirehose = False
imagesDir = "$HOME/.conky/imgs"
colorschemeGreyHex = "\\#606060"
colorschemeDimHex = "\\#3A3A3A"
colorschemeDarkHex = "\\#282828"
colorschemeWhiteHex = "\\#FFFFFF"
colorschemeRedHex = "\\#FF0000"
colorschemeGreenHex = "\\#006400"
colorschemeYellowHex = "\\#FFCC00"
conkyFile = tempfile.NamedTemporaryFile('w', delete = False).name
signal.signal(signal.SIGTERM, handleSigTERM)
sectionSpacing = "     \\\n"

f = open(conkyFile, 'w')

## CONKY SETTINGS
f.write("background yes\n")
f.write("out_to_console yes\n")
f.write("out_to_x no\n")
f.write("update_interval 1\n")
f.write("\nTEXT\n")

## Machine info
distro = "arch" # TODO
updateCheckInterval = "60"
f.write("^fg({})${{nodename}}\\\n".format(colorschemeGreyHex))
if distro == "arch":
    f.write("${{if_match ${{texeci {} /usr/bin/checkupdates | wc -l}} > 0}}\\\n".format(updateCheckInterval))
elif distro == "centos":
    f.write("${{if_match ${{texeci {} /usr/bin/yum check-update >/dev/null 2>&1; echo $?}} == 100}}\\\n".format(updateCheckInterval))
else:
    f.write("${if_match '' == 'x'}\\\n")

f.write("^fg()\\\n")
f.write("${endif}\\\n")
if distro == "arch":
    f.write(" ^i({}/arch.xbm) \\\n".format(imagesDir))
else:
    f.write(" ^c(7) \\\n")
f.write("^fg({})${{kernel}}\\\n".format(colorschemeGreyHex))
f.write("  \\\n")

## NETWORK
# Retrieve interfaces
tempFile = tempfile.NamedTemporaryFile()
os.system("ip link show up | egrep '^[0-9]*:' | awk -F: '$2 !~ /lo/ {{ sub(\"^ *\", \"\", $2); print $2 }}' > {}".format(tempFile.name))
interfaces = [line.strip() for line in tempFile]
tempFile.close()

# For each interface, generate conky output
for interface in [intf.decode("utf-8") for intf in interfaces]:
    f.write("  ${{if_up {}}}^fg()\\\n".format(interface))
    if interface[0] == "w":
        # f.write("Steve Taylor's Guest Network \\\n")
        f.write("${{wireless_essid {}}} \\\n".format(interface))
        f.write("^fg({})\\\n".format(colorschemeWhiteHex))
        f.write("${{if_match ${{wireless_link_qual_perc {}}} >= 95}}^i({}/wifi_100.xbm)${{else}}\\\n".format(interface, imagesDir))
        f.write("${{if_match ${{wireless_link_qual_perc {}}} >= 75}}^i({}/wifi_75.xbm)${{else}}\\\n".format(interface, imagesDir))
        f.write("${{if_match ${{wireless_link_qual_perc {}}} >= 50}}^i({}/wifi_50.xbm)${{else}}\\\n".format(interface, imagesDir))
        f.write("^i({}/wifi_25.xbm)\\\n".format(imagesDir))
        f.write("${endif}${endif}${endif} \\\n")
    elif interface[0] == "e":
        f.write("^fg()\\\n")
        f.write("^i({}/ethernet.xbm)\\\n".format(imagesDir))
    f.write("^fg({})${{addr {}}}^fg({}) \\\n".format(colorschemeGreyHex, interface, colorschemeWhiteHex))
    f.write("^fg({})${{if_match ${{downspeedf {}}} > 1.5}}^fg()${{endif}}\\\n".format(colorschemeDimHex, interface))
    f.write("^i({}/net_down.xbm)\\\n".format(imagesDir))
    f.write("^fg({})${{if_match ${{upspeedf {}}} > 1.5}}^fg()${{endif}}\\\n".format(colorschemeDimHex, interface))
    f.write("^i({}/net_up.xbm)\\\n".format(imagesDir))
    f.write("${endif}\\\n")

# Lastly, output the external IP
f.write("  ^fg({})".format(colorschemeGreyHex))
f.write("(${texeci 3 wget -q -O /dev/stdout http://checkip.dyndns.org/ | cut -d : -f 2- | cut -d \\< -f -1 | awk '{ print $1 }'})")
f.write(sectionSpacing)

## MEDIA
f.write("${if_match ${texeci 2 amixer get Master | egrep '(Mono|Front)' | tail -1 | grep -c '\[off\]'} >= 1}\\\n")
f.write("^fg({})${{else}}^fg()${{endif}}".format(colorschemeDimHex))
volumeSteps = [100, 94, 88, 82, 75, 69, 63, 56, 50, 44, 38, 31, 25, 19, 12, 6]
for i in volumeSteps:
    f.write("${{if_match ${{texeci 1 amixer get Master | tail -1 | sed 's/.*\[\([0-9]*\)%\].*/\\1/g'}} >= {}}}^i({}/volume_{}.xbm)${{else}}\\\n".format(i, imagesDir, i))
f.write("^i({}/volume_0.xbm)\\\n".format(imagesDir))
for i in range(len(volumeSteps)):
    f.write("${endif}")
f.write(sectionSpacing)

# CPU
numCpus = subprocess.check_output("grep -c 'processor' /proc/cpuinfo", shell=True).strip().decode("utf-8")
for cpu in range(1,int(numCpus)+1):
    f.write("^fg({})\\\n".format(colorschemeDimHex))
    f.write("${{if_match ${{cpu cpu{}}} > 50}}^fg({})${{endif}}\\\n".format(cpu, colorschemeGreyHex))
    f.write("${{if_match ${{cpu cpu{}}} >= 85}}^fg({})${{endif}}\\\n".format(cpu, colorschemeRedHex))
    f.write("${{if_match ${{cpu cpu{}}} < 15}}^fg({})${{endif}}\\\n".format(cpu, colorschemeDarkHex))
    f.write("^i({}/cpu.xbm) \\\n".format(imagesDir))

# RAM
f.write("^fg({})\\\n".format(colorschemeGreyHex))
f.write("${{if_match ${{memperc}} < 50}}^fg({})${{endif}}\\\n".format(colorschemeDimHex))
f.write("${{if_match ${{memperc}} < 25}}^fg({})${{endif}}\\\n".format(colorschemeDarkHex))
f.write("${{if_match ${{memperc}} >= 70}}^fg({})${{endif}}\\\n".format(colorschemeWhiteHex))
f.write("${{if_match ${{memperc}} >= 85}}^fg({})${{endif}}\\\n".format(colorschemeRedHex))
f.write(" ^i({}/mem.xbm)\\\n".format(imagesDir))
f.write("  \\\n")

# TEMP
f.write("^fg({})\\\n".format(colorschemeDarkHex))
f.write("${{if_match ${{acpitemp}} > 65}}^fg({})${{else}}\\\n".format(colorschemeWhiteHex))
f.write("${{if_match ${{acpitemp}} > 85}}^fg({})${{endif}}${{endif}}\\\n".format(colorschemeRedHex))
f.write("^i({}/temp.xbm)\\\n".format(imagesDir))
f.write("  \\\n")

# FAN
f.write("^fg({})\\\n".format(colorschemeDarkHex))
f.write("${{if_match ${{ibm_fan}} > 3000}}^fg({})${{else}}\\\n".format(colorschemeDimHex))
f.write("${{if_match ${{ibm_fan}} > 3500}}^fg({})${{else}}\\\n".format(colorschemeWhiteHex))
f.write("${{if_match ${{ibm_fan}} > 4000}}^fg({})${{endif}}${{endif}}${{endif}}\\\n".format(colorschemeRedHex))
f.write("^i({}/fan.xbm)\\\n".format(imagesDir))
f.write(sectionSpacing)

## TIME
f.write("^fg({})".format(colorschemeGreyHex))
f.write("${{time %a}} ^fg()${{time %d}} ^fg({})${{time %b}} \\\n".format( colorschemeDimHex))
f.write("^fg({})${{time %H%M}}^fg({})\\\n".format(colorschemeWhiteHex, colorschemeWhiteHex))
f.write("  ^fg({})${{uptime}}^fg({})\\\n".format(colorschemeDimHex, colorschemeWhiteHex))
f.write(sectionSpacing)

## BATTERY
f.write("^fg({})\\\n".format(colorschemeDarkHex))
f.write("${if_match ${battery_percent} < 99}^fg()${endif}\\\n")
f.write("${{if_match ${{battery_percent}} < 50}}^fg({})${{endif}}\\\n".format(colorschemeYellowHex))
f.write("${{if_match ${{battery_percent}} < 20}}^fg({})${{endif}}\\\n".format(colorschemeRedHex))
# f.write("${if_match ${battery_percent} < 10}${blink !}${endif}\\\n")
batterySteps = [100, 94, 88, 82, 75, 69, 63, 56, 50, 44, 38, 31, 25, 19, 12, 6]
for i in batterySteps:
    f.write("${{if_match ${{battery_percent}} >= {}}}^i({}/battery_{}.xbm)${{else}}\\\n".format(i, imagesDir, i))
f.write("^i({}/battery_0.xbm)\\\n".format(imagesDir))
for i in range(len(batterySteps)):
    f.write("${endif}")
f.write("\\\n")

f.write("^fg()")
# f.write("   ${fs_free_perc /}\\\n")

f.close()

for out in open(conkyFile):
    print(out, end="")
cleanup()

