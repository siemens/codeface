#! /usr/bin/env bash

#==================
#   Configure
#==================
BASEDIR=/home/wolfgang/projects/swi/prosoda/cluster
GITDIR=/home/wolfgang/git-repos/
PROJECT=linux
#TAG="tag"
TAG="non_tag"
#=================================================================

CLUSTER=${BASEDIR}/cluster.py
CONV=${BASEDIR}/conv.py
PERSONS=${BASEDIR}/persons.r
REPORT=${BASEDIR}/create_report.pl
GITREPO=${GITDIR}/${PROJECT}/.git
TAG_OPT="--${TAG}"

for i in "$@"; do
    VERSION=v2.6.$((i+1))
    echo "Processing ${VERSION}"

    ${CLUSTER} ${GITREPO} ${PROJECT} ${BASEDIR}/res/${PROJECT}/${TAG} \
	v2.6.${i} v2.6.$((i+1)) v2.6.$((i+1))-rc1 ${TAG_OPT} --create_db

    ${PERSONS} ${BASEDIR}/res/${PROJECT}/${TAG}/${VERSION} ${TAG_OPT}

    (cd ${BASEDIR}/res/${PROJECT}/${TAG}/${VERSION};
	for file in `ls sg*.dot wt*.dot`; do 
	    basefile=`basename $file .dot`; 
	    echo "Processing $file"; 
	    cat $file | ${CONV} | sfdp -Tpdf -Gcharset=latin1 > ${basefile}.pdf; 
	done)

    if [ ! -d "${BASEDIR}/res/${PROJECT}/${TAG}/latex" ]; then
	   mkdir ${BASEDIR}/res/${PROJECT}/${TAG}/latex
    fi

    ${REPORT} ${BASEDIR}/res/${PROJECT}/${TAG}/${VERSION} "${i}..$((i+1))" > ${BASEDIR}/res/${PROJECT}/${TAG}/latex/report_${VERSION}.tex;
    (cd ${BASEDIR}/res/${PROJECT}/${TAG}/latex && \
	pdflatex -output-directory=${BASEDIR}/res/${PROJECT}/${TAG}/ \
	${BASEDIR}/res/${PROJECT}/${TAG}/latex/report_${VERSION}.tex)
done
