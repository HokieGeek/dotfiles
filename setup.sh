#!/bin/sh

[ $# -gt 0 ] && destination=$1 || destination=$HOME
dotfilesDir=$(cd $(dirname $0); pwd)

echo "Destination directory: $destination"

cd $dotfilesDir

ignores="`basename $0` README.md"
for df in `ls -1`; do
    [ `echo ${ignores} | grep -c "${df}"` -eq 0 ] && {
        echo -n " Linking "
        [ -d ${df} ] && echo -n "directory" || echo -n "file"
        echo " ${df}"

        target="${destination}/.${df}"

        [ -L ${target} ] && {
            echo -n "   "
            rm -i ${target}
        }

        if [ -L ${target} ]; then
            echo "Did not create link for ${df}" 
        else
            ln -s ${dotfilesDir}/${df} ${target}
        fi
    }
done
