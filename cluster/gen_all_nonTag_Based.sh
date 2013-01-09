#! /usr/bin/env sh

BASEDIR=/Users/Mitchell/Documents/workspace/prosoda_repo/cluster
CLUSTER=/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/cluster.py
CONV=/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/conv.py
PERSONS=/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/persons.r
REPORT=/Users/Mitchell/Documents/workspace/prosoda_repo/cluster/create_report_nonTag.pl

for i in "$@"; do
    echo "Processing ${i}"
    ${CLUSTER} /Users/Mitchell/git/linux-2.6/.git ${BASEDIR}/res_NonTag/ ${i} --nonTag
    ${PERSONS} ${BASEDIR}/res_NonTag/${i} nonTag
    (cd ${BASEDIR}/res_NonTag/$i;
	for file in `ls sg*.dot wt*.dot`; do 
	    basefile=`basename $file .dot`; 
	    echo "Processing $file"; 
	    cat $file | ${CONV} | sfdp -Tpdf -Gcharset=latin1 > ${basefile}.pdf; 
	done)


    if [[ ! (-d "${BASEDIR}/res_NonTag/latex") ]]; then
        mkdir ${BASEDIR}/res_NonTag/latex
    fi

    ${REPORT} ${BASEDIR}/res_NonTag/${i} "${i}..$((i+1))" > ${BASEDIR}/res_NonTag/latex/report_${i}.tex;
    (cd ${BASEDIR/res_NonTag/latex} && pdflatex -output-directory=${BASEDIR}/res_NonTag ${BASEDIR}/res_NonTag/latex/report_${i}.tex)
done