#!/bin/bash
here=$(cd $(dirname $0); pwd)

${here}/statusbar.py > /tmp/xmonad.conkyrc && \
    conky -b -c /tmp/xmonad.conkyrc | dzen2 \
    -y '0' -x '0' -w `" ++ screenwidth_cmd ++ "` -h '" ++ barheight ++ \
    "' -ta 'r' -bg '" ++ colorBackground ++ "' -fg '" ++ colorForeground ++ 
    "' -fn '" ++ font ++ "'"
