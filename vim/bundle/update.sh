#!/bin/bash

[ "$1" != "--isagent" ] && exec ssh-agent $0 --isagent $@
shift
ssh-add ~/.ssh/id_rsa_github

[ $# -le 0 -o "$1" != "--theirs" ] && theirs="" || theirs="-v"

for r in `ls`; do
    [ ! -d $r -o ! -f "$r/.git" ] && continue
    pushd $r 2>&1 >/dev/null
    remote=`git remote -v | awk '$3 ~ /(fetch)/ { print $2 }'`
    [ `echo $remote | grep -c ${theirs} HokieGeek` -gt 0 ] && {
        echo "<<$r>>"
        git stash
        git fetch --all
        git pull --rebase origin master
        git stash pop
    }
    popd 2>&1 >/dev/null
done
