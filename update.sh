#!/bin/sh

[ `ps aux | grep ssh-agent | grep -v grep | wc -l` -le 0 ] && exec ssh-agent $0 $@
ssh-add

git fetch --all
git pull origin master
cd vim/bundle
git pull origin master
./update.sh --mine
./update.sh --theirs
