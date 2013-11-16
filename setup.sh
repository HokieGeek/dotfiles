#!/bin/bash

[ $# -gt 0 ] && destinationDir=$1 || destinationDir=$HOME
dotfilesDir=$(cd $(dirname $0); pwd)

echo "Destination directory: $destinationDir"

cd $dotfilesDir

doLink() {
    dfpath=$1
    target=$2
    df=`basename ${dfpath}`

    # Output what is being linked
    echo -n "Linking "
    [ -d ${df} ] && echo -n "directory" || echo -n "file"
    echo " ${df}"


    ## If a file already exists with this name
    # If it's a symlink
    [ -L ${target} ] && {
        # Ignore if it's just a link to the respective dotfiles file. Ask to delete it otherwise
        foundTarget="`ls -la ${target} | awk -F'->' '{ print $2 }' | cut -d' ' -f2`"
        [ $foundTarget = "${dfpath}" ] && {
            echo "  Already exists"
            continue
        } || {
            echo -n "   "
            rm -i ${target}
        }
    }

    # If a file still exists (directory, symlink, whatever) then create a backup of it
    [ -e ${target} ] && {
        echo "  Backing up existing"
        mv ${target}{,.orig}
    }

    # Will not attempt to create the link if a file of the same name still exists after all of that
    [ -e ${target} ] && {
        echo "Did not create link for ${df}" 
    } || {
        #echo "$target" 
        [ ! -d `dirname ${target}` ] && mkdir -p ${target}
        ln -s ${dfpath} ${target}
    }
}

ignores='`basename $0` README.md *~'
for df in `ls -1`; do
    # If the file is in the ignores list, then just skip the rest
    [ `echo ${ignores} | grep -c "${df}"` -eq 0 ] || continue

    [ "$df" = "config" ] && {
        echo "You need to deal with this"
        #echo "ok: ${df}"
        #for cf in `ls -1 ${df}`; do
        #    echo "cf: $cf"
        #    doLink ${dotfilesDir}/${df}/${cf} "${destinationDir}/.config/${cf}"
        #done
    } || doLink ${dotfilesDir}/${df} "${destinationDir}/.${df}"
done
