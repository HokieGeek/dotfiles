#!/bin/bash

# exec >> /tmp/xmonadstatusbar 2>&1
# echo "$@"

here=$(cd $(dirname $0); pwd)
tmprc=/tmp/xmonad.conkyrc

width=1
height=1
fg="#ffffff"
bg="#000000"
font="whatever"

while [ $# -gt 0 ]; do
    case $1 in
        --width) width=$2; shift ;;
        --height) height=$2; shift ;;
        --fg) fg=$2; shift ;;
        --bg) bg=$2; shift ;;
        --font) font=$2; shift ;;
        # *) usage ;;
    esac
    shift
done

# set -x
${here}/statusbar.py > ${tmprc} && {
conky -b -c ${tmprc} | dzen2 -y '0' -x '0' -ta 'r' \
    -w ${width} -h ${height} \
    -bg "${bg}" -fg "${fg}" \
    -fn "${font}"
}
