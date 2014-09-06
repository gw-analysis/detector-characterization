#!/bin/sh
set -e
#******************************************#
#     File Name: aldo.sh
#        Author: Takahiro Yamamoto
# Last Modified: 2014/09/06 22:39:21
#******************************************#

START=842747904
FINISH=842760192
GPS=${START}

make -f ./cuiSrm.mk

while test ${GPS} -le 842752000
do
    ./cuiSrm L1:LOSC-STRAIN 4096 ${GPS} ${GPS}
    GPS=`expr ${GPS} + 4096`
done

