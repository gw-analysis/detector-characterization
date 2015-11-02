#!/bin/sh


###  Parameters
DEBUG_MODE=1

CMD_PARA="/home/yamamoto/apps/parallel/bin/parallel"
CMD_HTML="./genDailySummaryPage"

MAX_CORE=8
MULTI="dailyCoherenceMon"
HTML_NCOL=3
MAIN_CH="K1:PEM-EX_REF"

YESTERDAY=`date -d '1 day ago' "+%Y %m %d"`
LOG_FILE="`date -d '1 day ago' "+%Y-%m-%d"`.log"
DAILY_DIR=${HOME}/public_html/`date -d '1 day ago' "+%Y/%m/%d/"`
#####  for test
# YESTERDAY="2015 07 17"
# LOG_FILE="dailyMon-2015-07-17.log"
# DAILY_DIR="2015/07/17/"


###  Main
if test ! ${1}
then 
    echo "Usage: dailyAll subSystem_name"
    exit 1
else
    CHANNELS=`cat ch_${1}.lst`
    MONITORS=`cat mon_${1}.lst`
fi

if test ${DEBUG_MODE} = "1"
then
    DEBUG="--dry-run"
    MKDIR_CMD="echo mkdir -p ${HOME}/public_html/${DAILY_DIR}"
    MVPNG_CMD="echo mv ./*.png ${HOME}/public_html/${DAILY_DIR}"
else
    MKDIR_CMD="mkdir -p ${HOME}/public_html/${DAILY_DIR}"
    MVPNG_CMD="mv ./*.png ${HOME}/public_html/${DAILY_DIR}"
fi

if test ${MAX_CORE} -a ${MAX_CORE} -ge 1
then
    JOB_NUM="--jobs ${MAX_CORE}"
    JOB_NUM_MUL="--jobs `expr ${MAX_CORE} / 2`"
fi

if test ${LOG_FILE}
then
    LOGGING="--joblog dailyMon-s_${LOG_FILE}"
    LOGGING_MUL="--joblog dailyMon-m_${LOG_FILE}"
fi

if test ${DEBUG_MODE} = "1"
then
    echo "${CMD_HTML} ${DAILY_DIR} `echo ${YESTERDAY} | sed -e 's/ /-/g'` ch_${1}.lst mon_${1}.lst ${1} ${HTML_NCOL}"
elif test -f ${CMD_HTML}
then
    ${CMD_HTML} ${DAILY_DIR} `echo ${YESTERDAY} | sed -e 's/ /-/g'` ch_${1}.lst mon_${1}.lst ${1} ${HTML_NCOL}
else
    echo "Can't find ${CMD_HTML}"
    exit 1
fi

if test -e ${CMD_PARA}
then
    if test "${MONITORS}"
    then
	for MON in ${MONITORS}
	do
	    if test "${MULTI}"
	    then
		for multi in ${MULTI}
		do
		    if test ${multi} != ${MON}
		    then
			monitors="${monitors} ${MON}"
		    fi
		done
	    else
		monitors="${monitors} ${MON}"
	    fi
	done
    fi
    if test "${monitors}" -a "${CHANNELS}"
    then
	${CMD_PARA} ${DEBUG} ${JOB_NUM} ${LOGGING} "./{1} ${YESTERDAY} {2}" ::: ${monitors} ::: ${CHANNELS}
	${MKDIR_CMD}
	${MVPNG_CMD}
    fi
    if test "${multi}" -a "${CHANNELS}"
    then
	${CMD_PARA} ${DEBUG} ${JOB_NUM_MUL} ${LOGGING_MUL} "./{1} ${YESTERDAY} ${MAIN_CH} {2}" ::: ${multi} ::: ${CHANNELS}
	${MKDIR_CMD}
	${MVPNG_CMD}
    fi
else
    echo "Can't find ${CMD_PARA}"
    exit 1
fi

exit 0

