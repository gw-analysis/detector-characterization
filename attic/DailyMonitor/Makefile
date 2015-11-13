#******************************************#
#     File Name: Makefile
#        Author: Takahiro Yamamoto
# Last Modified: 2015/11/13 21:02:48
#******************************************#

MAKE=make -f

# program
TAR1= SRMon
TAR2= RMon
TAR3= RMSMon
TAR4= CoherenceMon
TAR5= genDailySummaryPage
TAR6= Spectrum
TAR7= Spectrogram
TAR8= TimeSeries
TAR9= Bruco
TAR10= LT
TAR11= SensMon
TAR12= genDailyCmd
TARs= ${TAR1} ${TAR2} ${TAR4} ${TAR6} ${TAR7} ${TAR8} ${TAR3} ${TAR5} ${TAR9} ${TAR10} ${TAR11} ${TAR12}

# compile rule
all: ${TARs}

% : %.mk %.hs
	${MAKE} $<

clean:
	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi

cleanall: clean
	rm -f ${TARs}

