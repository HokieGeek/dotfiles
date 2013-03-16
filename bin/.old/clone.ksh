#! /bin/ksh
#------------------------------------------------------/
# Filename: $RCSfile$
# 
# author: per47214c
#
# Clones a disk
#
# $Log$
#------------------------------------------------------/

## VARIABLES
OUTPUT=/tmp/restore.out.$
NOTIFY=0
RESTART=1 ## The default is to restart on exit
SOURCE=$1
TARGET=$2
VFSTAB=/mtn/etc/vfstab
VFSTMP=/tmp/vfstab.$$
SEDXPR="s/`echo $SOURCE | cut -d'd' -f1`/`echo $TARGET | cut -d'd' -f1`/g"

## USAGE Warning
if [ $# -lt 2 ]; then
	echo "Usage: `basename $0` [-r] SOURCE_SLICE TARGET_SLICE [NOTIFY_ADDRESS ...]
	         -r   -- DISABLES system restarting when replication ends"
	exit 2
fi

## Argument Handler
if [ $1 = "-r" ]; then
	RESTART=0 ## Disables restarting
	shift
fi

## Shift the command-line arguments and determine if there are any email addresses
shift; shift;
if [ $# -gt 0 ]; then ## Save the email addresses and 
	NOTIFY=1
	ADDRESSES=""
	while [ $# -gt 0 ]; do
		ADDRESSES="$1 $ADDRESSES"
		shift
	done
	MAILTMP="/tmp/mailtmp.$$"
fi

## Define the notification function
doNotify() {
	echo "> Notifying replication end to '$ADDRESSES'" | tee $OUTPUT
	mailx -s "Replication ended on '`uname -n`'" $ADDRESSES < $OUTPUT 
}

## Send out the notification on error 
[ $NOTIFY -ne 0 ] && trap "echo 'ERROR: a call exited with an error' | tee $OUTPUT; doNotify; exit" ERR

## Warn that the system will be restarted
[ $RESTART -ne 0 ] && echo "WARNING: The system will be restarted after a successful replication" | tee $OUTPUT

## Set some shell options
#set -x ## Debug mode
set -e ## Exit on error

## Perform an fsck on the source drive
echo "> Checking integrity of '/dev/dsk/$SOURCE'"
fsck -y /dev/dsk/$SOURCE | tee $OUTPUT

## Determine start time
echo "> Replication start time: `date`" | tee $OUTPUT

## Perform the data dump
echo "> Replicating '/dev/dsk/$SOURCE' on '/dev/dsk/$TARGET'"
dd if=/dev/dsk/$SOURCE of=/dev/dsk/$TARGET bs=2048k | tee $OUTPUT

## Determine end time
echo "> Replication end time: `date`" | tee $OUTPUT

## Perform an fsck on the target drive
echo "> Checking integrity of '/dev/dsk/$TARGET'"
for i in 1 2 3; do fsck -y /dev/dsk/$TARGET; done | tee $OUTPUT

## Mount the target drive
mount /dev/dsk/$TARGET /mnt | tee $OUTPUT

## "Fix" entries in vfstab
echo "> Updating target drive filesystem mount information"
sed -e $SEDXPR $VFSTAB > $VFSTMP | tee $OUTPUT
mv $VFSTMP $VFSTAB | tee $OUTPUT

## Remove the copy of this script on the target drive
echo "> Cleaning up"
rm -rf /mnt/`basename $0` | tee $OUTPUT

## Unmount the target drive
umount /mnt | tee $OUTPUT

## Send out notification
[ $NOTIFY -ne 0 ] && doNotify

## Restart the system
[ $RESTART -ne 0 ] && init 6 
