#!/bin/bash

# exec 2>&1 >>/tmp/dk

awk '
$0 ~ /^--/ { printf("^fg(blue)%s^fg()\n", $0); next }
NR > 1 && $0 !~ /^ *$/ { sub("^[^ \t]*", "^fg(green)&^fg()"); print; next }
{ print $0 }
' $(cd $(dirname $0); pwd)"/keybindings-help.txt" | \
dzen2 -e \
'onstart=uncollapse,scrollhome,grabkeys;entertitle=grabkeys;enterslave=grabkeys;leaveslave=ungrabkeys;button2=exit:13;key_k=scrollup:39;key_j=scrolldown:39;key_Escape=ungrabkeys,exit' \
 -p -l 40 -x 800 -y 100 -w 620
