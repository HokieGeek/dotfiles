#!/bin/bash

here=$(cd `dirname $0`; pwd)

cd ${here}

echo ":: Installing the themes"
for t in `find . -maxdepth 1 -type d ! -name "." -print | cut -d/ -f2`; do
    echo -e "    $t"
    sudo cp -r ${here}/${t} /usr/share/slim/themes
done

echo ":: Editing slim.conf"
themes=`find . -maxdepth 1 -type d ! -name "." -print | cut -d/ -f2 | paste -d, -s`
sudo sed -i -e '/current_theme/{s/^#\s*//;s/default/'${themes}'/}' /etc/slim.conf
