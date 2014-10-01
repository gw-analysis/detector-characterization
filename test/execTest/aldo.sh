#!/bin/sh
set -e
#******************************************#
#     File Name: aldo.sh
#        Author: Takahiro Yamamoto
# Last Modified: 2014/09/11 22:19:46
#******************************************#

START=842747904
#START=842752000
FINISH=842760192
GPS=${START}

make -f ./cuiSrm.mk

# while test ${GPS} -le 842760192
# do
#     ./cuiSrm L1:LOSC-STRAIN 4096 ${GPS} ${START}
#     GPS=`expr ${GPS} + 4096`
# done

i=0
while test ${i} -ne 313
do
    ./cuiSrm
    i=`expr ${i} + 1`
done

