#!/bin/sh

[ $# -gt 0 ] && destination=$1 || destination=$HOME
dotfilesDir=$(cd $(dirname $0); pwd)

echo "Destination directory: $destination"

cd $dotfilesDir

ignores="`basename $0` README.md"
for df in `ls -1`; do
    [ `echo ${ignores} | grep -c "${df}"` -eq 0 ] && {
        echo "  Linking ${df}"
        ln -s ${dotfilesDir} ${destination}/.${df}
    }
done
