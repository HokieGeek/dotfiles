#!/bin/sh

## Add the needed variables
PROPS_FILE="PMDataHandler.properties";
#CSV_LOCATION=`grep CSV_LOCATION ${PROPS_FILE} | cut -d= -f2`;
CSV_LOCATION=/tmp/ops_pm_files;
PM_TARGET_SITE=`grep PM_TARGET_SITE ${PROPS_FILE} | cut -d= -f2`;
PM_TARGET_PREFIX=`grep PM_TARGET_PREFIX ${PROPS_FILE} | cut -d= -f2 | awk '{ print $1 }'`;

cd ${CSV_LOCATION};

for dslam in `fgrep -h DSLAM: * | cut -d: -f2 | cut -d, -f1`; do
	for csv IN `fgrep -l ${dslam} *`; do
		timestamp=`fgrep -h TIMESTAMP ${csv} | cut -d: -f2 | awk '{ print $1 }'`;
		mv -f ${csv} ${PM_TARGET_SITE}${PM_TARGET_PREFIX}${dslam}"/process/passive."${timestamp};
	done
	process_now ${dslam}
done

