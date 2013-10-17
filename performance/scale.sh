#! /bin/bash
# Test the scalability of prosoda
# Copyright Siemens AG 2013, Wolfgang Mauerer
#
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

if [ $# -ne 6 ]; then
    echo "Usage: $0 project cores.min cores.max outdir resdir repodir";
    exit -1;
fi

PROJECT=$1
CORES_MIN=$2
CORES_MAX=$3
OUTDIR=$4
RESDIR=$5
REPODIR=$6

mkdir -p ${OUTDIR}
rm -rf ${OUTDIR}/scale_${PROJECT}.txt

for CORES in `seq ${CORES_MAX} -1 ${CORES_MIN}`; do
    /usr/bin/time --format "%U %S %E %P" -o /tmp/time.txt \
	./perf_meas.sh ${PROJECT} ${CORES} ${OUTDIR} ${RESDIR} ${REPODIR}
  echo -n "${CORES} " >> ${OUTDIR}/scale_${PROJECT}.txt;
  cat /tmp/time.txt | head -n 1 >> ${OUTDIR}/scale_${PROJECT}.txt;
 done
