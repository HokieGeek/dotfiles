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

def determineDistro():
    if os.path.isfile("/etc/arch-release"):
        return "arch"
    elif os.path.isfile("/etc/centos-release"):
        return "centos"
    elif os.path.isfile("/etc/redhat-release"):
        return "redhat"
    elif os.path.isfile("/etc/lsb-release"):
        return "something" # TODO
    else:
        return "unknown"

## Setup cleanup
def cleanup():
    os.remove(conkyFile)

def handleSigTERM():
    cleanup()

## Variables
infoFirehose = False
imagesDir = "{}/imgs".format(os.path.dirname(os.path.realpath(__file__)))
height = int(args["height"])
colorschemeFgHex     = "\\{}".format(args["color_fg"])
colorschemeBgHex     = "\\{}".format(args["color_bg"])
colorschemeGreyHex   = "\\#606060"
colorschemeDimHex    = "\\#3a3a3a"
colorschemeDarkHex   = "\\#282828"
colorschemeWhiteHex  = "\\#cdcdcd"
colorschemeRedHex    = "\\#ff0000"
colorschemeGreenHex  = "\\#006400"
colorschemeYellowHex = "\\#ffcc00"
sectionSpacing       = "       \\\n"

conkyFile = tempfile.NamedTemporaryFile('w', delete = False).name
signal.signal(signal.SIGTERM, handleSigTERM)
f = open(conkyFile, 'w')

## CONKY SETTINGS
f.write("conky.config = {\n")
f.write("\tout_to_console = true,\n")
f.write("\tout_to_x = false,\n")
f.write("\tupdate_interval = 1,\n")
f.write("};\n")
f.write("\nconky.text = [[\n")

## Set the background
f.write("^bg({})\\\n".format(colorschemeBgHex))

## Machine info
distro = determineDistro()
updateCheckInterval = "1800"
f.write("^fg({})${{nodename}}\\\n".format(colorschemeGreyHex))
f.write("^fg({})".format(colorschemeDimHex))
if distro == "arch":
    f.write("${{if_match ${{texeci {} /usr/bin/checkupdates | wc -l}} > 0}}\\\n".format(updateCheckInterval))
elif distro == "centos":
    f.write("${{if_match ${{texeci {} /usr/bin/yum check-update >/dev/null 2>&1; echo $?}} == 100}}\\\n".format(updateCheckInterval))
else:
    f.write("${if_match '' == 'x'}\\\n")

f.write("^fg({})\\\n".format(colorschemeFgHex))
f.write("${endif}\\\n")
if distro == "arch":
    f.write(" ^i({}/arch.xbm) \\\n".format(imagesDir))
elif distro == "centos":
    f.write(" ^i({}/centos.xbm) \\\n".format(imagesDir))
else:
    f.write(" ^c(7) \\\n")
f.write("^fg({})${{kernel}}\\\n".format(colorschemeGreyHex))
f.write("   \\\n")

## NETWORK
# Retrieve interfaces
tempFile = tempfile.NamedTemporaryFile()
os.system("ip link show | awk -F': ' '/<BROADCAST/ {{ print $2 }}' > {}".format(tempFile.name))
interfaces = [line.strip() for line in tempFile]
tempFile.close()

# For each interface, generate conky output
for interface in [intf.decode("utf-8") for intf in interfaces]:
    f.write("${{if_up {}}}^fg({})\\\n".format(interface, colorschemeFgHex))

    if interface[0] == "w":
        f.write("${{wireless_essid {}}} \\\n".format(interface))
        # f.write("^fg({})\\\n".format(colorschemeWhiteHex))
        # f.write("${{if_match ${{wireless_link_qual_perc {}}} >= 95}}^i({}/wifi_100.xbm)${{else}}\\\n".format(interface, imagesDir))
        # f.write("${{if_match ${{wireless_link_qual_perc {}}} >= 75}}^i({}/wifi_75.xbm)${{else}}\\\n".format(interface, imagesDir))
        # f.write("${{if_match ${{wireless_link_qual_perc {}}} >= 50}}^i({}/wifi_50.xbm)${{else}}\\\n".format(interface, imagesDir))
        # f.write("^i({}/wifi_25.xbm)\\\n".format(imagesDir))
        # f.write("${endif}${endif}${endif} \\\n")
    elif interface[0] == "e":
        f.write("^fg({})\\\n".format(colorschemeFgHex))
        f.write("^i({}/ethernet.xbm)\\\n".format(imagesDir))
#     else:
#         f.write("${endif}\\\n")
#         continue
    f.write("^fg({})${{addr {}}}^fg({}) \\\n".format(colorschemeGreyHex, interface, colorschemeWhiteHex))
    f.write("^fg({})${{if_match ${{downspeedf {}}} > 1.5}}^fg({})${{endif}}\\\n".format(colorschemeDimHex, interface, colorschemeFgHex))
    f.write("^i({}/net_down.xbm)\\\n".format(imagesDir))
    f.write("^fg({})${{if_match ${{upspeedf {}}} > 1.5}}^fg({})${{endif}}\\\n".format(colorschemeDimHex, interface, colorschemeFgHex))
    f.write("^i({}/net_up.xbm)\\\n".format(imagesDir))
    f.write("${endif}  \\\n")

# Lastly, output the external IP
f.write("  ^fg({})".format(colorschemeDimHex))
#f.write("${texeci 5 wget -q -O /dev/stdout http://checkip.dyndns.org/ | awk -F\": \" '{ sub(\"<.*$\", \"\", $2); print $2 }' }")
f.write(sectionSpacing)

## MEDIA
volumeScriptCmd = "{}/volume.py --height={} --color-fg='{}' --color-bg='{}' --color-muted='{}'".format(os.path.dirname(os.path.realpath(__file__)), height, colorschemeFgHex.replace("\\", ""), colorschemeBgHex.replace("\\", ""), colorschemeDarkHex.replace("\\", ""))
f.write("^p(;-1)^fg({})\\\n".format(colorschemeBgHex))
f.write("${{execi 1 {}}}\\\n".format(volumeScriptCmd))
f.write("^p()^fg({})^bg({})\\\n".format(colorschemeFgHex, colorschemeBgHex))
f.write(sectionSpacing)

# CPU
numCpus = subprocess.check_output("grep -c 'processor' /proc/cpuinfo", shell=True).strip().decode("utf-8")
for cpu in range(1,int(numCpus)+1):
    f.write("^fg({})\\\n".format(colorschemeDimHex))
    f.write("${{if_match ${{cpu cpu{}}} > 50}}^fg({})${{endif}}\\\n".format(cpu, colorschemeGreyHex))
    f.write("${{if_match ${{cpu cpu{}}} >= 85}}^fg({})${{endif}}\\\n".format(cpu, colorschemeRedHex))
    f.write("${{if_match ${{cpu cpu{}}} < 15}}^fg({})${{endif}}\\\n".format(cpu, colorschemeDarkHex))
    f.write("^i({}/cpu.xbm) \\\n".format(imagesDir))
f.write("\\\n")

## RAM
f.write("^fg({})\\\n".format(colorschemeGreyHex))
f.write("${{if_match ${{memperc}} <= 50}}^fg({})${{endif}}\\\n".format(colorschemeDimHex))
f.write("${{if_match ${{memperc}} < 25}}^fg({})${{endif}}\\\n".format(colorschemeDarkHex))
f.write("${{if_match ${{memperc}} > 50}}^fg({})${{endif}}\\\n".format(colorschemeWhiteHex))
f.write("${{if_match ${{memperc}} >= 85}}^fg({})${{endif}}\\\n".format(colorschemeRedHex))
f.write(" ^i({}/mem.xbm)\\\n".format(imagesDir))
f.write("\\\n  \\\n")

## TEMP
f.write("^fg({})\\\n".format(colorschemeDarkHex))
f.write("${{if_match ${{acpitemp}} > 65}}^fg({})${{else}}\\\n".format(colorschemeWhiteHex))
f.write("${{if_match ${{acpitemp}} > 85}}^fg({})${{endif}}${{endif}}\\\n".format(colorschemeRedHex))
f.write("^i({}/temp.xbm)\\\n".format(imagesDir))
f.write("  \\\n")

## FAN
if os.path.isfile("/proc/acpi/ibm/fan"):
    f.write("^fg({})\\\n".format(colorschemeDarkHex))
    f.write("${{if_match ${{ibm_fan}} > 3000}}^fg({})${{else}}\\\n".format(colorschemeDimHex))
    f.write("${{if_match ${{ibm_fan}} > 3500}}^fg({})${{else}}\\\n".format(colorschemeWhiteHex))
    f.write("${{if_match ${{ibm_fan}} > 4000}}^fg({})${{endif}}${{endif}}${{endif}}\\\n".format(colorschemeRedHex))
    f.write("^i({}/fan.xbm)\\\n".format(imagesDir))
f.write(sectionSpacing)

## TIME
f.write("^fg({})".format(colorschemeGreyHex))
f.write("${{time %a}} ^fg({})${{time %d}} ^fg({})${{time %b}} \\\n".format(colorschemeWhiteHex, colorschemeDimHex))
f.write("^fg({})${{time %H%M}}^fg({})\\\n".format(colorschemeFgHex, colorschemeFgHex))
f.write("  ^fg({})${{uptime}}^fg({})\\\n".format(colorschemeDimHex, colorschemeWhiteHex))
f.write(sectionSpacing)

## BATTERY
def hasBattery():
    return os.path.islink("/sys/class/power_supply/BAT0") or \
            os.path.isfile("/sys/class/power_supply/BAT0") or \
            os.path.islink("/sys/class/power_supply/BAT1") or \
            os.path.isfile("/sys/class/power_supply/BAT1")

if hasBattery():
    batteryWidth=16
    batteryStep = int(100 / height)
    batterySteps = list(range(batteryStep, 100, batteryStep))
    batteryHeight = 1
    batteryHeightStep = 1
    f.write("^fg({})\\\n".format(colorschemeDarkHex))
    f.write("${if_match ${battery_percent} < 100}\\\n")
    f.write("^p()^p(;-1)^fg({})\\\n".format(colorschemeDimHex))
    f.write("${if_existing /sys/class/power_supply/BAT0/status Discharging}\\\n")
    f.write("^bg({})\\\n".format(colorschemeRedHex))
    f.write("${else}\\\n")
    f.write("${{if_match ${{battery_percent}} < 100}}^bg({})${{endif}}\\\n".format(colorschemeFgHex))
    f.write("${{if_match ${{battery_percent}} < 50}}^bg({})${{endif}}\\\n".format(colorschemeYellowHex))
    f.write("${endif}\\\n")
    for perc in reversed(batterySteps):
        f.write("${{if_match ${{battery_percent}} > {}}}^r({}x{})${{else}}\\\n".format(perc, batteryWidth, batteryHeight))
        batteryHeight += batteryHeightStep
    f.write("^r({}x0)\\\n".format(batteryWidth))
    for i in range(len(batterySteps)):
        f.write("${endif}")
    f.write("\\\n")
    f.write("${else}\\\n")
    f.write("^fg({})^r({}x{})\\\n".format(colorschemeBgHex, batteryWidth, height))
    f.write("${endif}\\\n")

f.write("]];\n")

## Done and done
f.close()
for out in open(conkyFile):
    print(out, end="")
cleanup()
