#!/bin/sh

wget -q -O /dev/stdout http://checkip.dyndns.org/ | cut -d : -f 2- | cut -d \< -f -1 | awk '{ print $1 }' > $1