#!/bin/bash

statusbarHeight=$1; shift
screenWidth=$1; shift
lines=$1; shift
dzen_args=$@

width=460
xpos=$(( ${screenWidth} - ${width} ))
ypos=${statusbarHeight}
scroll_step=$(( ${lines} / 2 ))

sed -n -e '/-- Keybindings/,/^-- *}}/p' $HOME/.xmonad/xmonad.hs | \
awk '
BEGIN {
    prettierKeyLabels["apostrophe"] = "'"'"'"
    prettierKeyLabels["AudioMute"] = "Mute"
    prettierKeyLabels["AudioRaiseVolume"] = "RaiseVolume"
    prettierKeyLabels["AudioLowerVolume"] = "LowerVolume"
    prettierKeyLabels["Launch1"] = "BlackButton"
}
NR == 1 {
    print "::: Xmonad Keybindings :::"
    next
}
$0 ~ /^ *$/ { print; next }
$0 ~ /%SKIPHELP%/ || $0 ~ /^ *-- ,/ || $0 ~ /^ *-- }}}$/ { next }
$0 ~ /-- %HELP%/ {
    sub("^ *-- %HELP% ", "")
    sub("^[^ \t]*", "^fg(darkgreen)&^fg()")
    print
    next
}
$1 ~ /--/ && $2 ~ /[^}]/{
    sub("{{{", "")
    sub("^ *", "")
    sub(".*", "^fg(blue)&^fg()")
    print
}
$1 ~ /,/ {
    modifiers=""
    if ($0 ~ "modm")        { modifiers=modifiers "-mod" }
    if ($0 ~ "controlMask") { modifiers=modifiers "-Ctrl" }
    if ($0 ~ "shiftMask")   { modifiers=modifiers "-Shift" }
    if ($0 ~ "mod1Mask")    { modifiers=modifiers "-Alt" }
    sub("^-", "", modifiers)
    if (length(modifiers) > 0) { modifiers=modifiers "-" }

    keyPrefix = "xF86XK_"
    if ($0 !~ keyPrefix) { keyPrefix = "xK_" }
    key=substr($0, match($0, keyPrefix))
    key=substr(key, 0, match(key, /),/))
    sub(keyPrefix, "", key)
    sub(")$", "", key)

    if (key in prettierKeyLabels) { key = prettierKeyLabels[key] }

    printf("^fg(green)%s%s^fg()    ", modifiers, key)

    if ($0 ~ /--/) {
        sub("^.*--", "")
        print
    } else {
        printf("\n")
    }
}
' | \
dzen2 -e \
"onstart=uncollapse,scrollhome,grabkeys;entertitle=grabkeys;enterslave=grabkeys;leavetitle=ungrabkeys;leaveslave=ungrabkeys;leaveslave=ungrabkeys;button2=exit:13;key_k=scrollup:${scroll_step};key_j=scrolldown:${scroll_step};key_Escape=ungrabkeys,exit;key_q=ungrabkeys,exit" \
 -p -x ${xpos} -y ${ypos} -w ${width} -l ${lines} ${dzen_args} &
sleep 0.09s
xdotool mousemove $(( ${xpos} + ${width} - 1 )) ${ypos}
