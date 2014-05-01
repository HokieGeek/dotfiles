#!/usr/bin/python

import subprocess
import tempfile
import os

## Add the network info
# Retrieve interfaces
tempFile = tempfile.NamedTemporaryFile()
os.system("ip link show up | egrep '^[0-9]*:' | awk -F: '$2 !~ /lo/ { sub(\"^ *\", \"\", $2); print $2 }' > %s", tempFile.name)
interfaces = [line.strip() for line in tempFile]
tempFile.close()

# For each interface, generate conky output
for interface in interfaces:
    print("${if_up %s}\\", interface, end="")
    if interface[0] == "w":
        print("${wireless_essid %s} \\", interface, end="")
        print("^fg(\\#9F0AC4)\\", end="")
        print("${if_match ${wireless_link_qual_perc %s} >= 95}^i($HOME/.conky/imgs/wifi_100.xbm)${else}\\", interface, end="")
        print("${if_match ${wireless_link_qual_perc %s} >= 75}^i($HOME/.conky/imgs/wifi_75.xbm)${else}\\", interface, end="")
        print("${if_match ${wireless_link_qual_perc %s} >= 50}^i($HOME/.conky/imgs/wifi_50.xbm)${else}\\", interface, end="")
        print("^i($HOME/.conky/imgs/wifi_25.xbm)\\", end="")
        print("${endif}${endif}${endif}\\", end="")
        print("^fg(\\#FFFFFF) \\", end="")
    # elif interface[0] == "e":
        #TODO: print("^i($HOME/.conky/imgs/ethernet.xbm)\\", end="")
        #TODO: print("^fg(\\#006400)^i(DOWN) ${downspeed %s}^fg(\\#FFFFFF) \\", interface, end="")
        #TODO: print("^fg(\\#FF0000)^i(UP) ${upspeed %s}^fg(\\#FFFFFF) \\", interface, end="")
    print("^fg(\\#606060)${addr %s}^fg(\\#FFFFFF) \\", interface, end="")
    print("${endif}\\", end="")

# Lastly, output the external IP
print("^fg(\\#606060) (${exec wget -q -O /dev/stdout http://checkip.dyndns.org/ | cut -d : -f 2- | cut -d \\< -f -1 | awk '{ print $1 }'})^fg(\\#FFFFFF)\\", end="")

## Add the process and memory info
#^fg(\#606060)^i($HOME/.conky/imgs/cpu.xbm)^fg(\#FFFFFF) \
numCpus = subprocess.check_output("grep -c 'processor' /proc/cpuinfo", shell=True).strip().decode("utf-8")
for cpu in range(1,int(numCpus)+1):
    print("${{if_match ${{cpu cpu{}}} >= 85}}^fg(\\#FF0000)${{endif}}\\".format(cpu), end="")
    print("${{if_match ${{cpu cpu{}}} < 10}} ${{endif}}${{cpu cpu{}}}%^fg(\\#FFFFFF) \\".format(cpu, cpu), end="")

#TODO Add RAM
#^fg(\#737373)^i($HOME/.conky/imgs/mem.xbm)^fg(\#FFFFFF) \
#${if_match ${memperc} >= 85}^fg(\#FF0000)${endif}${memperc}%   \
#^fg(\#9F0AC4)\

##TODO Media
#${if_match ${texeci 1 amixer get Master | grep Mono: | grep -c "\[off\]"} >= 1}\
#^i($HOME/.conky/imgs/spkr_02.xbm)\
#^fg(\#3A3A3A) \
#${else}\
#^i($HOME/.conky/imgs/spkr_01.xbm)\
#^fg(\#FFFFFF) \
#${endif}\
#${texeci 1 amixer get Master | tail -1 | cut -d'[' -f2 | cut -d']' -f1}  \
#^fg(\#FFFFFF) \


##TODO Time
#${time %a}, ^fg(\#9F0AC4)${time %d}^fg(\#FFFFFF) ^fg(\#737373)${time %b}^fg(\#FFFFFF) \
#^fg(\#9F0AC4)${time %H%M}^fg(\#FFFFFF) \
#^fg(\#3A3A3A)(${uptime})^fg(\#FFFFFF)  \

##TODO Battery
#^fg(\#9F0AC4)\
#${if_match ${battery_percent} < 50}^fg(\#FFCC00)${endif}\
#${if_match ${battery_percent} < 20}^fg(\#FF0000)${endif}\
#${if_match ${battery_percent} >= 99}^i($HOME/.conky/imgs/battery_100.xbm)${else}\
#${if_match ${battery_percent} >= 94}^i($HOME/.conky/imgs/battery_94.xbm)${else}\
#${if_match ${battery_percent} >= 88}^i($HOME/.conky/imgs/battery_88.xbm)${else}\
#${if_match ${battery_percent} >= 82}^i($HOME/.conky/imgs/battery_82.xbm)${else}\
#${if_match ${battery_percent} >= 75}^i($HOME/.conky/imgs/battery_75.xbm)${else}\
#${if_match ${battery_percent} >= 69}^i($HOME/.conky/imgs/battery_69.xbm)${else}\
#${if_match ${battery_percent} >= 63}^i($HOME/.conky/imgs/battery_63.xbm)${else}\
#${if_match ${battery_percent} >= 56}^i($HOME/.conky/imgs/battery_56.xbm)${else}\
#${if_match ${battery_percent} >= 50}^i($HOME/.conky/imgs/battery_50.xbm)${else}\
#${if_match ${battery_percent} >= 44}^i($HOME/.conky/imgs/battery_44.xbm)${else}\
#${if_match ${battery_percent} >= 38}^i($HOME/.conky/imgs/battery_38.xbm)${else}\
#${if_match ${battery_percent} >= 31}^i($HOME/.conky/imgs/battery_31.xbm)${else}\
#${if_match ${battery_percent} >= 25}^i($HOME/.conky/imgs/battery_25.xbm)${else}\
#${if_match ${battery_percent} >= 19}^i($HOME/.conky/imgs/battery_19.xbm)${else}\
#${if_match ${battery_percent} >= 12}^i($HOME/.conky/imgs/battery_12.xbm)${else}\
#${if_match ${battery_percent} >= 6}^i($HOME/.conky/imgs/battery_6.xbm)\
#${else}^i($HOME/.conky/imgs/battery_0.xbm)\
#${endif}${endif}${endif}${endif}${endif}${endif}${endif}${endif}${endif}${endif}${endif}${endif}${endif}${endif}${endif}${endif}
