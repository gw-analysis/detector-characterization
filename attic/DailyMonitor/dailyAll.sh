#!/bin/sh


###  User Parameters
CHANNELS="K1:PEM-EX_ACC_NO2_X_FLOOR \
          K1:PEM-EX_ACC_NO2_X_FLOOR"

# CHANNELS="K1:PEM-EX_ACC_NO2_X_FLOOR \
# 	 K1:PEM-EX_ACC_NO2_Y_FLOOR \
# 	 K1:PEM-EX_ACC_NO2_Z_FLOOR \
#  	 K1:PEM-EX_MAG_X_FLOOR \
#  	 K1:PEM-EX_MAG_Y_FLOOR \
#  	 K1:PEM-EX_MAG_Z_FLOOR \
#  	 K1:PEM-EX_MIC_FLOOR"


MONITORS="./dailySRMon \
          ./dailyRMon"

MAX_CORE=4


###  Admin Parameters
YESTERDAY="2015 07 17"
#YESTERDAY=`date -d '1 day ago' "+%Y %m %d"`
CMD_PARA="/home/yamamoto/apps/parallel/bin/parallel"
LOG_FILE="dailyMon-`date -d '1 day ago' "+%Y-%m-%d"`.log"


###  Main
if test ${MAX_CORE} -a ${MAX_CORE} -ge 1
then
    JOB_NUM="--jobs ${MAX_CORE}"
fi

if test ${LOG_FILE}
then
    LOGGING="--joblog ${LOG_FILE}"
fi

${CMD_PARA} ${JOB_NUM} ${LOGGING} "{1} {2} ${YESTERDAY}" ::: ${MONITORS} ::: ${CHANNELS}
