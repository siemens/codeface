#! /usr/bin/env bash

#==================
#   Configure
#==================
BASEDIR=/home/wolfgang/projects/swi/prosoda/cluster
GITDIR=/home/wolfgang/git-repos/
PROJECT=linux
#=================================================================

CLUSTER=${BASEDIR}/cluster.py
CONV=${BASEDIR}/conv.py
PERSONS=${BASEDIR}/persons.r
REPORT=${BASEDIR}/create_report.pl
GITREPO=${GITDIR}/${PROJECT}/.git
TAG_OPT="non_tag"

for i in "$@"; do
    VERSION=v2.6.$((i+1))
    echo "Processing ${VERSION}"

    ${CLUSTER} ${GITREPO} ${PROJECT} ${BASEDIR}/res/${PROJECT}/tag \
	v2.6.${i} v2.6.$((i+1)) v2.6.$((i+1))-rc1 --create_db

    ${PERSONS} ${BASEDIR}/res/${PROJECT}/tag/${VERSION} tag

    (cd ${BASEDIR}/res/${PROJECT}/tag/${VERSION};
	for file in `ls sg*.dot wt*.dot`; do 
	    basefile=`basename $file .dot`; 
	    echo "Processing $file"; 
	    cat $file | ${CONV} | sfdp -Tpdf -Gcharset=latin1 > ${basefile}.pdf; 
	done)

    if [ ! -d "${BASEDIR}/res/${PROJECT}/tag/latex" ]; then
	   mkdir ${BASEDIR}/res/${PROJECT}/tag/latex
    fi

    ${REPORT} ${BASEDIR}/res/${PROJECT}/tag/${VERSION} "${i}..$((i+1))" > ${BASEDIR}/res/${PROJECT}/tag/latex/report_${VERSION}.tex;
    (cd ${BASEDIR}/res/${PROJECT}/tag/latex && \
	pdflatex -output-directory=${BASEDIR}/res/${PROJECT}/tag/ \
	${BASEDIR}/res/${PROJECT}/tag/latex/report_${VERSION}.tex)
done
