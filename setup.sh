#!/bin/sh

[ $# -gt 0 ] && destination=$1 || destination=$HOME
dotfilesDir=$(cd $(dirname $0); pwd)

echo "Destination directory: $destination"

cd $dotfilesDir

ignores='`basename $0` README.md *~'
for df in `ls -1`; do
    # If the file is in the ignores list, then just skip the rest
    [ `echo ${ignores} | grep -c "${df}"` -eq 0 ] || continue

    # Output was is being linked
    echo -n "Linking "
    [ -d ${df} ] && echo -n "directory" || echo -n "file"
    echo " ${df}"

    target="${destination}/.${df}"

    ## If a file already exists with this name
    # If it's a symlink
    [ -L ${target} ] && {
        # Ignore if it's just a link to the respective dotfiles file. Ask to delete it otherwise
        foundTarget="`ls -la ${target} | awk -F'->' '{ print $2 }' | cut -d' ' -f2`"
        [ $foundTarget = "${dotfilesDir}/${df}" ] && {
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
        ln -s ${dotfilesDir}/${df} ${target}
    }
done
