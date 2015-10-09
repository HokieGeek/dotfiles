#!/usr/bin/python

import subprocess
import argparse

parser = argparse.ArgumentParser(description='Outputs conky/dzen config for volume meter')
parser.add_argument('--height',
                    default='12',
                    help="The height of the image",
                    metavar="10")
parser.add_argument('--color-fg',
                    default="#FFFFFF",
                    help="The hex value of the foreground color (default: #FFFFFF)",
                    metavar="#000000")
parser.add_argument('--color-bg',
                    default="#000000",
                    help="The hex value of the background color (default: #000000)",
                    metavar="#000000")
parser.add_argument('--color-muted',
                    default="#c0c0c0",
                    help="The hex value of the color to use when sound is muted (default: #c0c0c0)",
                    metavar="#000000")
args = vars(parser.parse_args())

## Variables
height = int(args["height"])
colorschemeFgHex = "{}".format(args["color_fg"])
colorschemeBgHex = "{}".format(args["color_bg"])
colorschemeMuteHex = "{}".format(args["color_muted"])

volumeLevel = 0
volumeStep = 100 / (height*2.85)
volumeMuteCmd = "amixer get Master | egrep '^\s*(Mono|Front)' | tail -1 | awk -F'[' '{ sub(/]/, \"\", $NF); print $NF }'"
volumeLevelCmd = "amixer get Master -M | awk -F'[' '$2 ~ /%/ { sub(/%]/, \"\", $2); print $2 }' | head -1"
isMuted = subprocess.check_output(volumeMuteCmd, shell=True).strip().decode("utf-8") == "off"
level = int(subprocess.check_output(volumeLevelCmd, shell=True).strip().decode("utf-8"))
for i in reversed(range(0, height)):
    if isMuted:
        print("^bg({})".format(colorschemeMuteHex), end="")
    else:
        if level >= round(volumeLevel):
            print("^bg({})".format(colorschemeFgHex), end="")
        else:
            print("^bg({})".format(colorschemeMuteHex), end="")
    for j in range(0,2):
        print("^r(1x{})".format(i), end="")
        volumeLevel += volumeStep

    print("^bg({})^r(1x{})".format(colorschemeBgHex, i), end="")
    volumeLevel += volumeStep

