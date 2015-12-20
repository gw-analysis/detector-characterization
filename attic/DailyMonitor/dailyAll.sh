#!/bin/sh
#set -e

###  Parameters
DEBUG_MODE=1

CMD_PARA="/home/yamamoto/apps/parallel/bin/parallel"
CMD_HTML="./genDailySummaryPage"
CMD_PRINT="./genDailyCmd"

MAX_CORE=1
LARGE_MEM="dailyCoherenceMon dailyTimeSeriesMon"
HTML_NCOL=3
MAIN_CH="K1:PSL-PMC_TRANS_DC_OUT_DQ"

YESTERDAY=`date -d '1 day ago' "+%Y %m %d"`
DAILY_DIR=`date -d '1 day ago' "+%Y/%m/%d/"`
LOG_FILE="`date -d '1 day ago' "+%Y-%m-%d"`.log"

MIRROR_SERVER="detchar@seikai.hep.osaka-cu.ac.jp"

#####  for test
#YESTERDAY="2015 07 17"
#DAILY_DIR="2015/07/17/"
#LOG_FILE="2015-07-17.log"

###  Parameter check
if test ! ${1}
then 
    echo "Usage: dailyAll master.lst"
    exit 1
else
    MASTER=${1}
fi

if test ${DEBUG_MODE} = "1"
then
    DEBUG="--dry-run"
    MKDIR_CMD="${HOME}/public_html/${DAILY_DIR}"
    MVPNG_CMD="echo mv -f ./*.png ${HOME}/public_html/${DAILY_DIR}"
else
    MKDIR_CMD="${HOME}/public_html/${DAILY_DIR}"
    MVPNG_CMD="mv -f ./*.png ${HOME}/public_html/${DAILY_DIR}"
fi

if test ${MAX_CORE} -a ${MAX_CORE} -ge 1
then
    JOB_NUM="--jobs ${MAX_CORE}"
    JOB_NUM_MUL="--jobs `expr ${MAX_CORE} / 2`"
fi

if test ${LOG_FILE}
then
    LOGGING="--joblog dailyMon-s${1}_${LOG_FILE}"
    LOGGING_MUL="--joblog dailyMon-m${1}_${LOG_FILE}"
fi

#### Generate Web Page ####
printf "\n#### Generate Web Page\n"
x=`cat ${1}`
IFS=$'\n'
for y in $x
do
    z=${y%%.lst*}
    if test ${DEBUG_MODE} = "1"
    then
	echo "${CMD_HTML} ${DAILY_DIR} `echo ${YESTERDAY} | sed -e 's/ /-/g'` ${y} ${z#*_} ${HTML_NCOL}"
    elif test -f ${CMD_HTML}
    then
	echo "${CMD_HTML} ${DAILY_DIR} `echo ${YESTERDAY} | sed -e 's/ /-/g'` `echo ${y} | awk '{print $1}'` `echo ${y} | awk '{print $2}'` ${z#*_} ${HTML_NCOL}"
	${CMD_HTML} ${DAILY_DIR} `echo ${YESTERDAY} | sed -e 's/ /-/g'` `echo ${y} | awk '{print $1}'` `echo ${y} | awk '{print $2}'` ${z#*_} ${HTML_NCOL}
    else
	echo "Can't find ${CMD_HTML}"
	exit 1
    fi
done
IFS=$' '

#### Generate Execute Command ####
printf "\n#### Generate Execute Command\n"
if test -f ${CMD_PRINT}
then
    echo "${CMD_PRINT} ${1} ${YESTERDAY}"
    EXE_CMD=`${CMD_PRINT} ${1} ${YESTERDAY}`
else
    echo "Can't find ${CMD_PRINT}"
    exit 1
fi

#### Main ####
printf "\n#### Execute dailyMonitor\n"
if test "${EXE_CMD}"
then
    ${CMD_PARA} --noswap -d"\n" ${DEBUG} ${JOB_NUM} ${LOGGING} "./{1}" ::: "${EXE_CMD}"
    mkdir -p ${MKDIR_CMD}
    ${MVPNG_CMD}
    #### send results to seikai ####
    SSH_CMD="ssh ${MIRROR_SERVER} \"mkdir\" \"-p\" ${MKDIR_CMD}"
    SCP_CMD="scp ${MKDIR_CMD}* ${MIRROR_SERVER}:${MKDIR_CMD}"
    ${SSH_CMD}
    ${SCP_CMD}
else
    echo "empty: \${EXE_CMD}"
fi


exit 0

