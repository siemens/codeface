#! /usr/bin/env bash

#==================
#   Configure
#==================
BASEDIR=/Users/Mitchell/Documents/workspace/prosoda_repo/cluster
GITREPO=/Users/Mitchell/git/linux-2.6/.git
#=================================================================

CLUSTER=${BASEDIR}/cluster.py
CONV=${BASEDIR}/conv.py
PERSONS=${BASEDIR}/persons.r
REPORT=${BASEDIR}/create_report.pl

for i in "$@"; do
    echo "Processing ${i}"
    ${CLUSTER} ${GITREPO} ${BASEDIR}/res_Tag/ ${i} --create_db
    ${PERSONS} ${BASEDIR}/res_Tag/${i} tag
    (cd ${BASEDIR}/res_Tag/$i;
	for file in `ls sg*.dot wt*.dot`; do 
	    basefile=`basename $file .dot`; 
	    echo "Processing $file"; 
	    cat $file | ${CONV} | sfdp -Tpdf -Gcharset=latin1 > ${basefile}.pdf; 
	done)


    if [ ! -d "${BASEDIR}/res_Tag/latex" ]; then
	   mkdir ${BASEDIR}/res_Tag/latex
    fi

    ${REPORT} ${BASEDIR}/res_Tag/${i} "${i}..$((i+1))" > ${BASEDIR}/res_Tag/latex/report_${i}.tex;
    (cd ${BASEDIR}/res_Tag/latex && pdflatex -output-directory=${BASEDIR}/res_Tag ${BASEDIR}/res_Tag/latex/report_${i}.tex)
done