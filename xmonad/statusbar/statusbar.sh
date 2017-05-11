#!/bin/bash

# exec 2>&1 >>/tmp/xmsb
echo "ARGS: $@"

here=$(cd ${0%/*}; pwd)
tmprc=/tmp/xmonad.conkyrc

while (( $# > 0 )); do
    case $1 in
        --width)  width=$2 ;;
        --height) height=$2 ;;
        --xpos)   xpos=$2 ;;
        --fg)     fg=$2 ;;
        --bg)     bg=$2 ;;
        --font)   font=$2 ;;
    esac
    shift 2
done

[ -f /usr/local/bin/python3.5 ] && py="/usr/local/bin/python3.5" || py=$(which python)
${py} ${here}/statusbar.py --height ${height:-1} --color-fg ${fg:-"#ffffff"} --color-bg ${bg:-"#000000"} > ${tmprc} || exit 1
conky -c ${tmprc} | dzen2 -dock -y '0' -x ${xpos} -ta 'r' -w ${width:-1} -fn "${font:-'*'}" -bg ${bg}
