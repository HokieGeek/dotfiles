#!/bin/bash

[ $# -gt 0 ] && destinationDir=$1 || destinationDir=$HOME
dotfilesDir=$(cd $(dirname $0); pwd)
configDir="${destinationDir}/.config"

echo "Destination directory: $destinationDir"

cd $dotfilesDir

doLink() {
    dfpath=$1
    target=$2
    df=`basename ${dfpath}`
    # echo "doLink($1, $2)"

    # Output what is being linked
    echo -n "Linking "
    [ -d ${dfpath} ] && echo -n "directory " || echo -n "file "
    [ $target = $configDir ] && echo -n "config/"
    echo "${df}"

    [ "${target}" = "${configDir}" ] && trueTarget=${target}/${df} || trueTarget=$target
    # echo "tT = $trueTarget"

    ## If a file already exists with this name
    # If it's a symlink
    [ -L ${trueTarget} ] && {
        # Ignore if it's just a link to the respective dotfiles file. Ask to delete it otherwise
        foundTarget="`ls -la ${trueTarget} | awk -F'->' '{ print $2 }' | cut -d' ' -f2`"
        # echo "fT = $foundTarget"
        [ $foundTarget = "${dfpath}" ] && {
            echo "  Already exists"
            continue
        } || {
            echo -n "   "
            rm -i ${trueTarget}
        }
    }

    # If a file still exists (directory, symlink, whatever) then create a backup of it
    [ -e ${trueTarget} ] && {
        echo "  Backing up existing"
        mv ${trueTarget}{,.orig}
    }

    # Will not attempt to create the link if a file of the same name still exists after all of that
    [ -e ${trueTarget} ] && {
        echo "Did not create link for ${df}" 
        continue
    }

    #echo "$target" 
    #[ ! -d `dirname ${target}` ] && mkdir -p ${target}
    ln -s ${dfpath} ${target}
}

ignores='`basename $0` README.md *~'
for df in `ls -1`; do
    # If the file is in the ignores list, then just skip the rest
    [ `echo ${ignores} | grep -c "${df}"` -eq 0 ] || continue

    [ "$df" = "config" ] && {
        [ ! -d "${configDir}" ]  && mkdir ${configDir}
        # echo ">>>>>> ok: ${df}"
        for cf in `ls -1 ${df}`; do
            # echo "cf: $cf"
            doLink ${dotfilesDir}/${df}/${cf} ${configDir}
        done
    } || doLink ${dotfilesDir}/${df} "${destinationDir}/.${df}"
done
# ls ${destinationDir}*(-@)
