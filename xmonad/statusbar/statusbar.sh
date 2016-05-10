#!/bin/bash

# exec 2>&1 >>/tmp/xmsb
# echo "ARGS: $@"

here=$(cd $(dirname $0); pwd)
tmprc=/tmp/xmonad.conkyrc

width=1
height=1
fg="#ffffff"
bg="#000000"
font="*"

while [ $# -gt 0 ]; do
    case $1 in
        --width) width=$2; shift ;;
        --height) height=$2; shift ;;
        --xpos) xpos=$2; shift ;;
        --fg) fg=$2; shift ;;
        --bg) bg=$2; shift ;;
        --font) font=$2; shift ;;
        # *) usage ;;
    esac
    shift
done

[ -f /usr/local/bin/python3.5 ] && py="/usr/local/bin/python3.5" || py=`which python`
${py} ${here}/statusbar.py --height ${height} --color-fg ${fg} --color-bg ${bg} > ${tmprc} || exit 1
conky -c ${tmprc} | dzen2 -y '0' -x ${xpos} -ta 'r' -w ${width} -fn "${font}" -bg ${bg}
