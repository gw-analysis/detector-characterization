#!/bin/sh

DATE=2015-12-15
FILE=${DATE}_gps.txt
FILEPIC=${DATE}_gps.png
rm $FILE
for i in `~/exe/kagraDailyDataFind $DATE JST`
do
    VAL=`echo $i | cut -d "-" -f 3`
    VALHEAD=`echo $VAL | cut -c 1-4`
    VALTAIL=`echo $VAL | cut -c 5-11`
    echo "${VALTAIL}  1" >> ${FILE}

done

GPSSTART=`~/exe/localtime2gps ${DATE} 00:00:00 JST`
GPS1=`echo ${GPSSTART} | cut -d " " -f 6 | cut -c 5-11`
GPSEND=`~/exe/localtime2gps ${DATE} 24:00:00 JST`
GPS2=`echo ${GPSEND} | cut -d " " -f 6 | cut -c 5-11`

echo $GPS1
echo $GPS2

gnuplot <<EOF
set xrange [${GPS1}:${GPS2}]
set xlabel "GPS[s] - ${VALHEAD}000000"
set ylabel "available frame data"
set grid
set style fill solid border 
set boxwidth 1
set terminal png
set output "${FILEPIC}"
plot "${FILE}" u 1:2 with boxes lw 2 lc rgb "orange"
EOF

#rm ${FILE}