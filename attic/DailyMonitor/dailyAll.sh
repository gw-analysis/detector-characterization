#!/bin/sh
#set -e

###  Parameters
DEBUG_MODE=1

CMD_PARA="/home/yamamoto/apps/parallel/bin/parallel"
CMD_HTML="./genDailySummaryPage"
CMD_PRINT="./genDailyCmd"

MAX_CORE=7
LARGE_MEM="dailyCoherenceMon dailyTimeSeriesMon"
HTML_NCOL=3
MAIN_CH="K1:LSC-MICH_ERR_CAL_OUT_DQ"

YESTERDAY=`date -d '1 day ago' "+%Y %m %d"`
DAILY_DIR=`date -d '1 day ago' "+%Y/%m/%d/"`
LOG_FILE="`date -d '1 day ago' "+%Y-%m-%d"`.log"

MIRROR_SERVER="detchar@seikai.icrr.u-tokyo.ac.jp"

BRUCO_CHLIST="./chBruco.lst"


#####  for test
#YESTERDAY="2016 03 18"
#DAILY_DIR="2016/03/18/"
#LOG_FILE="2016-03-18.log"

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
    SSH_CMD="echo ssh ${MIRROR_SERVER} \"mkdir\" \"-p\" ${MKDIR_CMD}"
    SCP_CMD="echo scp ${MKDIR_CMD}* ${MIRROR_SERVER}:${MKDIR_CMD}"
    BRUCO_CMD="echo ./Bruco"
    BRUCOMVPNG_CMD="echo mv -f ./*_Bruco.png ${HOME}/public_html/${DAILY_DIR}"
    BRUCOMVHTML_CMD="echo mv -f ./*_Bruco.html ${HOME}/public_html/${DAILY_DIR}"
    SCP_BRUCO_CMD="echo scp ${MKDIR_CMD}*_Bruco* ${MIRROR_SERVER}:${MKDIR_CMD}"
else
    MKDIR_CMD="${HOME}/public_html/${DAILY_DIR}"
    MVPNG_CMD="mv -f ./*.png ${HOME}/public_html/${DAILY_DIR}"
    SSH_CMD="ssh ${MIRROR_SERVER} \"mkdir\" \"-p\" ${MKDIR_CMD}"
    SCP_CMD="scp ${MKDIR_CMD}* ${MIRROR_SERVER}:${MKDIR_CMD}"
    BRUCO_CMD="./Bruco"
    BRUCOMVPNG_CMD="mv -f ./*_Bruco.png ${HOME}/public_html/${DAILY_DIR}"
    BRUCOMVHTML_CMD="mv -f ./*_Bruco.html ${HOME}/public_html/${DAILY_DIR}"
    SCP_BRUCO_CMD="scp ${MKDIR_CMD}*_Bruco* ${MIRROR_SERVER}:${MKDIR_CMD}"
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
    ${SSH_CMD}
    ${SCP_CMD}
else
    echo "empty: \${EXE_CMD}"
fi


### Bruco ###
#Bruco yyyy mm dd ch.lst
#BRUCO_CMD="./Bruco"
#BRUCO_CHLIST="./chBruco.lst"
### execute Bruco
#${BRUCO_CMD} ${YESTERDAY} ${BRUCO_CHLIST}

### move production to html directory
#${BRUCOMVPNG_CMD}
#${BRUCOMVHTML_CMD}

### scopy production to the mirror server
#${SCP_BRUCO_CMD}


exit 0

