#!/bin/sh

output=mod_noise.txt

#awk -F , 'NR>1{print $1,$2}' Virgo10.txt > mod1_noise.txt
#awk -F , 'NR>1{print $1,$2}' Virgo10.txt > mod2_noise.txt
#awk -F , 'NR>1{print $1, sqrt($3*$3 + $7*$7 + $9*$9 + $10*$10)}' Virgo10.txt > mod2_noise.txt
#awk -F , 'NR>1{print $1, sqrt($3*$3 + $7*$7 + $10*$10)}' Virgo10.txt > mod3_noise.txt

LW="w l lw 4"
LW2="w l lw 4"

# gnuplot <<EOF
# set term eps fontscale 0.7 enhanced
# set output "noise_spectrum.eps"

# set log xy

# set xrange[1:8000]
# set yrange[1e-24:1e-10]

# plot \
# "mod3_noise.txt"     ${LW2} lc rgb "black" title "total",\
# "Virgo10.txt" u 1:3  ${LW} dt 2 lc rgb "magenta" title "seismic noise",\
# "Virgo10.txt" u 1:7  ${LW} dt 2 lc rgb "orange"  title "thermal noise(pendulum)",\
# "Virgo10.txt" u 1:10 ${LW} dt 2 lc rgb "blue"    title "shot noise"

# EOF

XLABEL="set xlabel\"f[Hz]\""
YLABEL="set ylabel\"h(f) [1/\sqrt(Hz)]\""

TERMINAL="set terminal eps enhanced"
LINETYPE="w l lw 5"
LINETYPE2="w l lw 2" 
FILE=mod4_noise.txt
awk -F , 'NR>1{print $1, $3, $4, $8, $10, $11, $14, $15, $16, $17, $18}' Virgo10.txt > $FILE

FILE2="mod4_noise_sn.txt"
awk -F , 'NR>1{print $1, sqrt($3*$3 + $4*$4 + $8*$8 + $10*$10 + $11*$11 + $14*$14 + $15*$15 + $16*$16 + $17*$17 + $18*$18)}' Virgo10.txt > $FILE2


gnuplot <<EOF
$XLABEL
$YLABEL

set log xy

set term postscript eps linewidth 1.3 fontscale 1.4 enhanced color size 6in,5in

#set key below horizontal maxcols 5 center
set key top right

set colorsequence podo
set format y "10^{%L}"
set xlabel "f"
set ylabel "|n(f)|"
set output "mod_noise.eps"
#plot [0.1:8192] [1e-27:1e-10]\

plot [1:8192] [1e-24:1e-17]\
"$FILE" u 1:2 ${LINETYPE2} dt 1 lc "blue" title "Seismic",\
"$FILE" u 1:3 ${LINETYPE2} dt 1 lc 2 title "Newtonian",\
"$FILE" u 1:4 ${LINETYPE2} dt 1 lc 5 title "Thermal Elastic Mirror",\
"$FILE" u 1:5 ${LINETYPE2} dt 1 lc 6 title "Shot noise",\
"$FILE" u 1:6 ${LINETYPE2} dt 1 lc 7 title "Radiation Pressure Noise",\
"$FILE" u 1:7 ${LINETYPE2} dt 1 lc 8 title "Acoustic",\
"$FILE" u 1:8 ${LINETYPE2} dt 3 lc 2 title "Distorsion",\
"$FILE" u 1:9 ${LINETYPE2} dt 3 lc 4 title "Coating phase reflectivity",\
"$FILE" u 1:10 ${LINETYPE2} dt 3 lc 5 title "Absorption",\
"$FILE" u 1:11 ${LINETYPE2} dt 3 lc 6 title "Magnetic",\
"" u 1:(sqrt(\$2*\$2 + \$3*\$3 + \$4*\$4 + \$5*\$5 + \$6*\$6 + \$7*\$7 + \$8*\$8 + \$9*\$9 + \$10*\$10 + \$11*\$11)) ${LINETYPE2} dt 1 lc 0 title "total"

#"$FILE" u 1:13 ${LINETYPE2} dt 2 lc 7 title "",\
#"$FILE" u 1:14 ${LINETYPE2} dt 2 lc 8 title "Absorption",\
#"$FILE" u 1:15 ${LINETYPE2} dt 4 lc 2 title "Magnetic",\
#"$FILE" u 1:16 w l lw 3 lc 0 title "Total"


EOF

