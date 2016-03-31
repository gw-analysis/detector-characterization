#!/bin/sh
#set -e

###  Parameters
DEBUG_MODE=0

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
    echo "Usage: dailyBruco chBruco.lst"
    exit 1
else
    MASTER=${1}
fi

if test ${DEBUG_MODE} = "1"
then
    DEBUG="--dry-run"
    MKDIR_CMD="${HOME}/public_html/${DAILY_DIR}"
    SSH_CMD="echo ssh ${MIRROR_SERVER} \"mkdir\" \"-p\" ${MKDIR_CMD}"
    BRUCO_CMD="echo ./Bruco"
    BRUCOMVPNG_CMD="echo mv -f ./*_Bruco.png ${HOME}/public_html/${DAILY_DIR}"
    BRUCOMVHTML_CMD="echo mv -f ./*_Bruco.html ${HOME}/public_html/${DAILY_DIR}"
    SCP_BRUCO_CMD="echo scp ${MKDIR_CMD}*_Bruco* ${MIRROR_SERVER}:${MKDIR_CMD}"
else
    MKDIR_CMD="${HOME}/public_html/${DAILY_DIR}"
    SSH_CMD="ssh ${MIRROR_SERVER} \"mkdir\" \"-p\" ${MKDIR_CMD}"
    BRUCO_CMD="./Bruco"
    BRUCOMVPNG_CMD="mv -f ./*_Bruco.png ${HOME}/public_html/${DAILY_DIR}"
    BRUCOMVHTML_CMD="mv -f ./*_Bruco.html ${HOME}/public_html/${DAILY_DIR}"
    SCP_BRUCO_CMD="scp ${MKDIR_CMD}*_Bruco* ${MIRROR_SERVER}:${MKDIR_CMD}"
fi


#### Main ####
printf "\n#### Execute dailyBruco\n"

#Bruco yyyy mm dd ch.lst
#BRUCO_CMD="./Bruco"
#BRUCO_CHLIST="./chBruco.lst"
### execute Bruco
${BRUCO_CMD} ${YESTERDAY} ${BRUCO_CHLIST}

### move production to html directory
${BRUCOMVPNG_CMD}
${BRUCOMVHTML_CMD}

### scopy production to the mirror server
${SCP_BRUCO_CMD}


exit 0



