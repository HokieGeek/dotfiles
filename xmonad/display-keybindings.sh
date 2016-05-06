#!/bin/bash

statusbarHeight=$1
screenWidth=$2
lines=$3
bg=$4

width=615
xpos=$(( ${screenWidth} - ${width} ))
ypos=${statusbarHeight}
scroll_step=$(( ${lines} / 2 ))

awk '
NR > 1 && $0 !~ /^ *$/ && $1 !~ /^--/ { sub("^[^ \t]*", "^fg(green)&^fg()"); }
$1 ~ /^--/ { sub(".*", "^fg(blue)&^fg()") }
{ print $0 }
' $(cd $(dirname $0); pwd)"/keybindings-help.txt" | \
dzen2 -e \
"onstart=uncollapse,scrollhome,grabkeys;entertitle=grabkeys;enterslave=grabkeys;leaveslave=ungrabkeys;button2=exit:13;key_k=scrollup:${scroll_step};key_j=scrolldown:${scroll_step};key_Escape=ungrabkeys,exit;key_q=ungrabkeys,exit" \
 -p -l ${lines} -x ${xpos} -y ${ypos} -w ${width} -bg ${bg} &
sleep 0.09s
xdotool mousemove $(( ${xpos} + ${width} - 1 )) ${ypos}
