#! /usr/bin/env sh

BASEDIR=/Users/Mitchell/Documents/workspace/prosoda_repo/cluster
CLUSTER=/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/cluster.py
CONV=/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/conv.py
PERSONS=/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/persons.r
REPORT=/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/create_report.pl

for i in "$@"; do
    echo "Processing ${i}"
    ${CLUSTER} /Users/Mitchell/git/linux-2.6/.git ${BASEDIR}/res/ ${i} --create_db
    ${PERSONS} ${BASEDIR}/res/${i} Tag
    (cd ${BASEDIR}/res/$i;
	for file in `ls sg*.dot wt*.dot`; do 
	    basefile=`basename $file .dot`; 
	    echo "Processing $file"; 
	    cat $file | ${CONV} | sfdp -Tpdf > ${basefile}.pdf; 
	done)


    if [[ ! (-d "${BASEDIR}/res/latex") ]]; then
	echo mkdir ${BASEDIR}/res/latex
    fi

    ${REPORT} ${BASEDIR}/res/${i} "${i}..$((i+1))" > ${BASEDIR}/res/latex/report_${i}.tex;
    (cd ${BASEDIR/res/latex} && pdflatex ${BASEDIR}/res/latex/report_${i}.tex)
done