#! /usr/bin/env sh

BASEDIR=/Users/wolfgang/papers/csd/cluster/
CLUSTER=/Users/wolfgang/papers/csd/cluster/cluster.py
CONV=/Users/wolfgang/papers/csd/cluster/conv.py
PERSONS=/Users/wolfgang/papers/csd/cluster/persons.r
REPORT=/Users/wolfgang/papers/csd/cluster/create_report.pl

for i in "$@"; do
    echo "Processing ${i}"
    ${CLUSTER} /Users/wolfgang/git-repos/linux/.git ${BASEDIR}/res/ ${i}
    ${PERSONS} ${BASEDIR}res/${i}/
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