#!/bin/sh


FILE1=$1
FILE2=$2

TERMINAL="set terminal png fontscale 1.4 size 800,800"

OUTPUTFILE=hoge.png

#NAME_DATA1=`/usr/bin/basename ${FILE1} .txt`
#NAME_DATA2=`/usr/bin/basename ${FILE2} .txt`
NAME_DATA1=${1%.*}
NAME_DATA2=`/usr/bin/basename ${FILE2} .txt`

KEY="${NAME_DATA1} - ${NAME_DATA2}"

XRANGE="set xrange[-5:5]"
YRANGE="set yrange[-5:5]"



TMPFILE=tmp.txt
paste $1 $2 > ${TMPFILE}

gnuplot <<EOF

${TERMINAL}
set output "${OUTPUTFILE}"

${XRANGE}
${YRANGE}

set xlabel "$1"
set ylabel "$2"
set size square

set view equal xy
plot "${TMPFILE}" with point pt 3 ps 1.5 lc 3 title "$KEY"
EOF

rm tmp.txt
mv ${OUTPUTFILE} ${NAME_DATA1}_${NAME_DATA2}.png
#open ${OUTPUTFILE}
