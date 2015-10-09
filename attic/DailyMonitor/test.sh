#!/bin/sh
#******************************************#
#     File Name: test.sh
#        Author: Takahiro Yamamoto
# Last Modified: 2015/10/02 00:19:09
#******************************************#


if test ! ${4}
then
    echo "Usage: hoge.sh monitor month startDay stopDay"
    exit 1
fi

CMD="/home/yamamoto/apps/parallel/bin/parallel"

CHANNELS="K1:PEM-EX_ACC_NO2_X_FLOOR \
	 K1:PEM-EX_ACC_NO2_Y_FLOOR \
	 K1:PEM-EX_ACC_NO2_Z_FLOOR \
	 K1:PEM-EX_MAG_X_FLOOR \
	 K1:PEM-EX_MAG_Y_FLOOR \
	 K1:PEM-EX_MAG_Z_FLOOR \
	 K1:PEM-EX_MIC_FLOOR"

MONITOR=${1}
month=${2}
daySrt=${3}
dayEnd=`expr ${4} + 1`

day=${daySrt}
while test ${day} -ne ${dayEnd}
do
    days="${days} ${day}"
    day=`expr ${day} + 1`
done

${CMD} --joblog "zzz.log" --jobs 49 "${MONITOR} 2015 ${month} {1} {2}" ::: ${days} ::: ${CHANNELS}

