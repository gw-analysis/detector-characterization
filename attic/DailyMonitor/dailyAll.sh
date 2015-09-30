#!/bin/sh


###  User Parameters
DEBUG_MODE=1
CHANNELS="K1:PEM-EX_ACC_NO2_X_FLOOR \
          K1:PEM-EX_ACC_NO2_Y_FLOOR"

# CHANNELS="K1:PEM-EX_ACC_NO2_X_FLOOR \
# 	 K1:PEM-EX_ACC_NO2_Y_FLOOR \
# 	 K1:PEM-EX_ACC_NO2_Z_FLOOR \
#  	 K1:PEM-EX_MAG_X_FLOOR \
#  	 K1:PEM-EX_MAG_Y_FLOOR \
#  	 K1:PEM-EX_MAG_Z_FLOOR \
#  	 K1:PEM-EX_MIC_FLOOR"

MONITORS="./dailySRMon \
          ./dailyRMon \
          ./dailyRMSMon"


###  Admin Parameters
MAX_CORE=3
YESTERDAY="2015 07 17"
#YESTERDAY=`date -d '1 day ago' "+%Y %m %d"`
LOG_FILE="dailyMon-2015-07-17.log"
# LOG_FILE="dailyMon-`date -d '1 day ago' "+%Y-%m-%d"`.log"
CMD_PARA="/home/yamamoto/apps/parallel/bin/parallel"

###  Main
if test ${DEBUG_MODE} = "1"
then
    DEBUG="--dry-run"
fi

if test ${MAX_CORE} -a ${MAX_CORE} -ge 1
then
    JOB_NUM="--jobs ${MAX_CORE}"
fi

if test ${LOG_FILE}
then
    LOGGING="--joblog ${LOG_FILE}"
fi

${CMD_PARA} ${DEBUG} ${JOB_NUM} ${LOGGING} "{2} {1} ${YESTERDAY}" ::: ${CHANNELS} ::: ${MONITORS}
