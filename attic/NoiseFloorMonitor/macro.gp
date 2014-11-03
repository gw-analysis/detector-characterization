set logscale y
set xr[0:2000]
plot "oneshot.dat" w l
replot "psd.dat" w l
