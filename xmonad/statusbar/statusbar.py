import subprocess
import tempfile
import os
import stat
import signal
import argparse
import sys

## Handle the arguments
# f = open('/tmp/statusbar.py.out', 'w+')
# f.write(" ".join(sys.argv))
parser = argparse.ArgumentParser(description='Create a conky config')
parser.add_argument('--height',
                    default='12',
                    help="The value in pixels of the height of the statusbar",
                    metavar="10")
parser.add_argument('--color-fg',
                    default="#FFFFFF",
                    help="The hex value of the foreground color (default: #FFFFFF)",
                    metavar="#000000")
parser.add_argument('--color-bg',
                    default="#000000",
                    help="The hex value of the background color (default: #000000)",
                    metavar="#000000")
args = vars(parser.parse_args())

## Setup cleanup
def cleanup():
    os.remove(conkyFile)

def handleSigTERM():
    cleanup()

## Variables
infoFirehose = False
imagesDir = "{}/imgs".format(os.path.dirname(os.path.realpath(__file__)))
height = int(args["height"])
colorschemeFgHex = "\\{}".format(args["color_fg"])
colorschemeBgHex = "\\{}".format(args["color_bg"])
colorschemeGreyHex = "\\#606060"
colorschemeDimHex = "\\#3a3a3a"
colorschemeDarkHex = "\\#282828"
colorschemeWhiteHex = "\\#ffffff"
colorschemeRedHex = "\\#ff0000"
colorschemeGreenHex = "\\#006400"
colorschemeYellowHex = "\\#ffcc00"
sectionSpacing = "       \\\n"

conkyFile = tempfile.NamedTemporaryFile('w', delete = False).name
signal.signal(signal.SIGTERM, handleSigTERM)
f = open(conkyFile, 'w')

## CONKY SETTINGS
# f.write("background yes\n")
f.write("out_to_console yes\n")
f.write("update_interval 1\n")
f.write("\nTEXT\n")

## Set the background
f.write("^bg({})\\\n".format(colorschemeBgHex))

## Machine info
distro = "arch" # TODO
updateCheckInterval = "60"
f.write("^fg({})${{nodename}}\\\n".format(colorschemeGreyHex))
f.write("^fg({})".format(colorschemeFgHex))
if distro == "arch":
    f.write("${{if_match ${{texeci {} /usr/bin/checkupdates | wc -l}} > 0}}\\\n".format(updateCheckInterval))
elif distro == "centos":
    f.write("${{if_match ${{texeci {} /usr/bin/yum check-update >/dev/null 2>&1; echo $?}} == 100}}\\\n".format(updateCheckInterval))
else:
    f.write("${if_match '' == 'x'}\\\n")

f.write("^fg({})\\\n".format(colorschemeFgHex))
f.write("${endif}\\\n")
if distro == "arch":
    f.write("^ca(1, st -e yaourt -Syua)")
    f.write(" ^i({}/arch.xbm) \\\n".format(imagesDir))
    f.write("^ca()")
else:
    f.write("^ca(1, st -e sudo yum -y update)")
    f.write(" ^c(7) \\\n")
    f.write("^ca()")
f.write("^fg({})${{kernel}}\\\n".format(colorschemeGreyHex))
f.write("  \\\n")

## NETWORK
# Retrieve interfaces
tempFile = tempfile.NamedTemporaryFile()
# TODO: this is ugly
os.system("netstat -i | awk '$NF ~ /R/ && $1 !~ /lo/ {{ print $1 }}' > {}".format(tempFile.name))
# f.write(tempFile)
interfaces = [line.strip() for line in tempFile]
tempFile.close()

# For each interface, generate conky output
for interface in [intf.decode("utf-8") for intf in interfaces]:
    f.write("  ${{if_up {}}}^fg({})\\\n".format(interface, colorschemeFgHex))

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
        f.write("^fg({})\\\n".format(colorschemeFgHex))
        f.write("^i({}/ethernet.xbm)\\\n".format(imagesDir))
    f.write("^fg({})${{addr {}}}^fg({}) \\\n".format(colorschemeGreyHex, interface, colorschemeWhiteHex))
    f.write("^fg({})${{if_match ${{downspeedf {}}} > 1.5}}^fg({})${{endif}}\\\n".format(colorschemeDimHex, interface, colorschemeFgHex))
    f.write("^i({}/net_down.xbm)\\\n".format(imagesDir))
    f.write("^fg({})${{if_match ${{upspeedf {}}} > 1.5}}^fg({})${{endif}}\\\n".format(colorschemeDimHex, interface, colorschemeFgHex))
    f.write("^i({}/net_up.xbm)\\\n".format(imagesDir))
    f.write("${endif}\\\n")

# Lastly, output the external IP
f.write("  ^fg({})".format(colorschemeDimHex))
f.write("${texeci 3 wget -q -O /dev/stdout http://checkip.dyndns.org/ | cut -d : -f 2- | cut -d \< -f -1 | awk '{ print $1 }' | sed 's/null//' }")
f.write(sectionSpacing)

## MEDIA
f.write("^ca(1, st -e alsamixer)")
if True:
    f.write("${if_match ${texeci 2 amixer get Master | egrep '(Mono|Front)' | tail -1 | grep -c '\[off\]'} >= 1}\\\n")
    f.write("^fg({})${{else}}^fg({})${{endif}}\\\n".format(colorschemeDarkHex, colorschemeFgHex))
    volumeSteps = [100, 94, 88, 82, 75, 69, 63, 56, 50, 44, 38, 31, 25, 19, 12, 6]
    volumeCmd = "amixer get Master -M | awk -F'[' '$2 ~ /%/ { sub(/%]/, \"\", $2); print $2 }'"
    for i in volumeSteps:
        f.write("${{if_match ${{texeci 1 {}}} }} >= {} }}^i({}/volume_{}.xbm)${{else}}\\\n".format(volumeCmd, i, imagesDir, i))
    f.write("^i({}/volume_0.xbm)\\\n".format(imagesDir))
    for i in range(len(volumeSteps)):
        f.write("${endif}")
else:
    volumeFile = "/tmp/statusbar.py.vol"
    # TODO: volumeFile = tempfile.NamedTemporaryFile('w', delete = False).name

    volumeScriptName = tempfile.NamedTemporaryFile('w', delete = False).name
    volumeScript = open(volumeScriptName, 'w')
    volumeScript.write("#!/bin/bash\n")
    volumeScript.write("[ $# -le 0 ] && {\n")
    volumeScript.write("    {\n")
    volumeScript.write("        amixer get Master -M | awk -F'[' '$2 ~ /%/ { sub(/%]/, \"\", $2); print $2 }' | head -1\n")
    volumeScript.write("        amixer get Master | egrep '(Mono|Front)' | tail -1 | awk -F'[' '{ sub(/]/, \"\", $NF); print $NF }'\n")
    volumeScript.write("    }} > {}\n".format(volumeFile))
    volumeScript.write("    chmod 666 {}\n".format(volumeFile))
    volumeScript.write("} || {\n")
    volumeScript.write("    [ $1 -le `head -1 {}` -a \"`tail -1 {}`\" == \"on\" ] && echo 1 || echo 0\n".format(volumeFile, volumeFile))
    volumeScript.write("}\n")
    volumeScript.close()
    os.chmod(volumeScriptName, stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR | stat.S_IRGRP | stat.S_IXGRP | stat.S_IROTH | stat.S_IXOTH)
    f.write("${{texeci 1 {}}}\\\n".format(volumeScriptName))

    volumeLevelCmd = "amixer get Master -M | awk -F'[' '$2 ~ /%/ { sub(/%]/, \"\", $2); print $2 }' | head -1"
    volumeMuteCmd = "amixer get Master | egrep '(Mono|Front)' | tail -1 | awk -F'[' '{ sub(/]/, \"\", $NF); print $NF }'"

    volumeLevel = 0
    volumeStep = 100 / (height*2.82)
    # volumeStep = 2.95
    # f.write("\nAFP: step = {}\n".format(volumeStep))
    f.write("^p(;-1)^fg({})\\\n".format(colorschemeBgHex))
    for i in reversed(range(0, height)):
        for j in range(0,2):
            # f.write("${{if_match ${{texeci 1 {} {}}} == 1 }}^bg({})${{else}}^bg({})${{endif}}".format(volumeScriptName, round(volumeLevel), colorschemeFgHex, colorschemeDimHex))
            f.write("${{if_match ${{texeci 1 {}}} == 'off' }}^bg({})${{else}}\\\n".format(volumeMuteCmd, colorschemeDarkHex))
            f.write("${{if_match ${{texeci 1 {}}} >= {} }}^bg({})${{else}}^bg({})${{endif}}\\\n${{endif}}\\\n".format(volumeMuteCmd, round(volumeLevel), colorschemeFgHex, colorschemeDarkHex))
            f.write("^r(1x{})\\\n".format(i))
            volumeLevel += volumeStep

        # f.write("\nAFP: {}, {}, {}\n".format(volumeLevel, volumeLevel+volumeStep, volumeLevel+(volumeStep*2)))
        f.write("^bg({})^r(1x{})\\\n".format(colorschemeBgHex, i))
        volumeLevel += volumeStep
    f.write("^p()^fg({})^bg({})\\\n".format(colorschemeFgHex, colorschemeBgHex))
    # TODO: delete volumeScriptName?


    #volumeFile = "/tmp/statusbar.py.vol" ## TODO: betterify

    #volumeScriptName = tempfile.NamedTemporaryFile('w', delete = False).name
    #volumeScript = open(volumeScriptName, 'w')
    #volumeScript.write("#!/bin/bash\n")
    #volumeScript.write("[ $# -le 0 ] && {\n")
    #volumeScript.write("    {\n")
    #volumeScript.write("        amixer get Master -M | awk -F'[' '$2 ~ /%/ { sub(/%]/, \"\", $2); print $2 }' | head -1\n")
    #volumeScript.write("        amixer get Master | egrep '(Mono|Front)' | tail -1 | awk -F'[' '{ sub(/]/, \"\", $3); print $3 }'\n")
    #volumeScript.write("    }} > {}\n".format(volumeFile))
    #volumeScript.write("    chmod 666 {}\n".format(volumeFile))
    #volumeScript.write("} || {\n")
    #volumeScript.write("    set -x\n")
    #volumeScript.write("    [ $1 -ge `head -1 {}` -a \"`tail -1 {}`\" == \"on\" ] && echo 1 || echo 0\n".format(volumeFile, volumeFile))
    #volumeScript.write("}\n")
    #volumeScript.close()
    #os.chmod(volumeScriptName, stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR | stat.S_IRGRP | stat.S_IXGRP | stat.S_IROTH | stat.S_IXOTH)
    #f.write("${{texeci 1 {}}}\\\n".format(volumeScriptName))

    #volumeLevel = 0
    ## volumeStep = int(100 / (height*3))
    #volumeStep = 3.036
    ## f.write("\nAFP: step = {}\n".format(volumeStep))
    #f.write("^p(;-1)^fg({})\\\n".format(colorschemeBgHex))
    #for i in reversed(range(0, height)):
    #    for j in range(0,2):
    #        f.write("${{if_match ${{texeci 1 {} {}}} == 1 }}^bg({})${{else}}^bg({})${{endif}}".format(volumeScriptName, round(volumeLevel), colorschemeDimHex, colorschemeFgHex))
    #        f.write("^r(1x{})\\\n".format(i))
    #        volumeLevel += volumeStep

    #    # f.write("\nAFP: {}, {}, {}\n".format(volumeLevel, volumeLevel+volumeStep, volumeLevel+volumeStep))
    #    f.write("^bg({})^r(1x{})\\\n".format(colorschemeBgHex, i))
    #    volumeLevel += volumeStep
    #f.write("^p()^fg({})^bg({})\\\n".format(colorschemeFgHex, colorschemeBgHex))

f.write("^ca()")
f.write(sectionSpacing)

# CPU
f.write("^ca(1, htop --sort-key PERCENT_CPU)\\\n")
numCpus = subprocess.check_output("grep -c 'processor' /proc/cpuinfo", shell=True).strip().decode("utf-8")
for cpu in range(1,int(numCpus)+1):
    f.write("^fg({})\\\n".format(colorschemeDimHex))
    f.write("${{if_match ${{cpu cpu{}}} > 50}}^fg({})${{endif}}\\\n".format(cpu, colorschemeGreyHex))
    f.write("${{if_match ${{cpu cpu{}}} >= 85}}^fg({})${{endif}}\\\n".format(cpu, colorschemeRedHex))
    f.write("${{if_match ${{cpu cpu{}}} < 15}}^fg({})${{endif}}\\\n".format(cpu, colorschemeDarkHex))
    f.write("^i({}/cpu.xbm) \\\n".format(imagesDir))
f.write("^ca()\\\n")

## RAM
f.write("^ca(1, htop --sort-key PERCENT_MEM)\\\n")
f.write("^fg({})\\\n".format(colorschemeGreyHex))
f.write("${{if_match ${{memperc}} <= 50}}^fg({})${{endif}}\\\n".format(colorschemeDimHex))
f.write("${{if_match ${{memperc}} < 25}}^fg({})${{endif}}\\\n".format(colorschemeDarkHex))
f.write("${{if_match ${{memperc}} > 50}}^fg({})${{endif}}\\\n".format(colorschemeWhiteHex))
f.write("${{if_match ${{memperc}} >= 85}}^fg({})${{endif}}\\\n".format(colorschemeRedHex))
f.write(" ^i({}/mem.xbm)\\\n".format(imagesDir))
f.write("^ca()\\\n")
f.write("  \\\n")

## TEMP
f.write("^fg({})\\\n".format(colorschemeDarkHex))
f.write("${{if_match ${{acpitemp}} > 65}}^fg({})${{else}}\\\n".format(colorschemeWhiteHex))
f.write("${{if_match ${{acpitemp}} > 85}}^fg({})${{endif}}${{endif}}\\\n".format(colorschemeRedHex))
f.write("^i({}/temp.xbm)\\\n".format(imagesDir))
f.write("  \\\n")

## FAN
# f.write("${acpifan}\\\n")
if os.path.isfile("/proc/acpi/ibm/fan"):
    f.write("^fg({})\\\n".format(colorschemeDarkHex))
    f.write("${{if_match ${{ibm_fan}} > 3000}}^fg({})${{else}}\\\n".format(colorschemeDimHex))
    f.write("${{if_match ${{ibm_fan}} > 3500}}^fg({})${{else}}\\\n".format(colorschemeWhiteHex))
    f.write("${{if_match ${{ibm_fan}} > 4000}}^fg({})${{endif}}${{endif}}${{endif}}\\\n".format(colorschemeRedHex))
    f.write("^i({}/fan.xbm)\\\n".format(imagesDir))
f.write(sectionSpacing)

## TIME
f.write("^fg({})".format(colorschemeGreyHex))
f.write("${{time %a}} ^fg({})${{time %d}} ^fg({})${{time %b}} \\\n".format(colorschemeFgHex, colorschemeDimHex))
f.write("^fg({})${{time %H%M}}^fg({})\\\n".format(colorschemeWhiteHex, colorschemeWhiteHex))
f.write("  ^fg({})${{uptime}}^fg({})\\\n".format(colorschemeDimHex, colorschemeWhiteHex))
f.write(sectionSpacing)

## BATTERY
f.write("^fg({})\\\n".format(colorschemeDarkHex))
f.write("${{if_match ${{battery_percent}} < 99}}^fg({})${{endif}}\\\n".format(colorschemeFgHex))
f.write("${{if_match ${{battery_percent}} < 50}}^fg({})${{endif}}\\\n".format(colorschemeYellowHex))
f.write("${{if_match ${{battery_percent}} < 20}}^fg({})${{endif}}\\\n".format(colorschemeRedHex))

batteryWidth=16
batteryStep = int(100 / height)
batterySteps = list(range(batteryStep, 100, batteryStep))
batteryHeight = 1
batteryHeightStep = 1
# f.write("^fg(\#151515)${battery_percent}^fg()")
f.write("${if_match ${battery_percent} < 100}\\\n")
f.write("^p()^p(;-1)^fg({})^bg({})\\\n".format(colorschemeDimHex, colorschemeFgHex))
for perc in reversed(batterySteps):
    f.write("${{if_match ${{battery_percent}} > {}}}^r({}x{})${{else}}\\\n".format(perc, batteryWidth, batteryHeight))
    batteryHeight += batteryHeightStep
f.write("^r({}x0)\\\n".format(batteryWidth))
for i in range(len(batterySteps)):
    f.write("${endif}")
f.write("\\\n")
f.write("${else}\\\n")
f.write("^fg({})^r({}x{})\\\n".format(colorschemeDarkHex, batteryWidth, height))
f.write("${endif}\\\n")

## Done and done
f.close()
for out in open(conkyFile):
    print(out, end="")
cleanup()
