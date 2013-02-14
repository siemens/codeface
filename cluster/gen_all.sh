#! /usr/bin/env bash

#==================
#   Configure
#==================
BASEDIR=${BASEDIR:-/home/wolfgang/projects/swi/prosoda/cluster}
GITDIR=${GITDIR:-/home/wolfgang/git-repos/}
PROJECT=${PROJECT:-linux}
TAG=${TAG:-tag}

# Nothing customisable below here
#=================================================================
CLUSTER=${BASEDIR}/cluster.py
CONV=${BASEDIR}/conv.py
PERSONS=${BASEDIR}/persons.r
REPORT=${BASEDIR}/create_report.pl
GITREPO=${GITDIR}/${PROJECT}/.git
TAG_OPT="--${TAG}"

# Parameter sanity checks
if [[ "$TAG" != "tag" && "$TAG" != "non_tag" ]]
then
    echo "Please specify either 'tag' or 'non_tag' for TAG.";
    exit -1;
fi

if [ ! -e ${GITDIR} ]
then
    echo "Please specify a valid git repository.";
    exit -1;
fi

if [[ "$#" -ne 2 && "$#" -ne 3 ]]
then
    echo "Usage: $0 start_rev end_rev <rc_start>";
    exit -1;
fi

START_REV=$1
END_REV=$2
RC_START=$3

echo "Processing ${PROJECT} (${START_REV}..${END_REV})"
echo "-> Preparing clustering input"
${CLUSTER} ${GITREPO} ${PROJECT} ${BASEDIR}/res/${PROJECT}/${TAG} \
    ${START_REV} ${END_REV} ${RC_START} ${TAG_OPT} --create_db

echo "-> Detecting clusters"
${PERSONS} ${BASEDIR}/res/${PROJECT}/${TAG}/${END_REV} ${TAG_OPT}

echo "-> Generating cluster graphs"
(cd ${BASEDIR}/res/${PROJECT}/${TAG}/${END_REV};
    for file in `ls sg*.dot wt*.dot`; do
	basefile=`basename $file .dot`;
	cat $file | ${CONV} | sfdp -Tpdf -Gcharset=latin1 > ${basefile}.pdf;
	done)

echo "-> Creating summary report"
if [ ! -d "${BASEDIR}/res/${PROJECT}/${TAG}/latex" ]; then
    mkdir ${BASEDIR}/res/${PROJECT}/${TAG}/latex
fi

${REPORT} ${BASEDIR}/res/${PROJECT}/${TAG}/${END_REV} "${i}..$((i+1))" > ${BASEDIR}/res/${PROJECT}/${TAG}/latex/report_${END_REV}.tex;
(cd ${BASEDIR}/res/${PROJECT}/${TAG}/latex && \
    pdflatex -output-directory=${BASEDIR}/res/${PROJECT}/${TAG}/ \
    ${BASEDIR}/res/${PROJECT}/${TAG}/latex/report_${END_REV}.tex)
