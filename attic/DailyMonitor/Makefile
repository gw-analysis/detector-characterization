#******************************************#
#     File Name: Makefile
#        Author: Takahiro Yamamoto
# Last Modified: 2015/11/09 16:30:09
#******************************************#

MAKE=make -f

# program
TAR1= dailySRMon
TAR2= dailyRMon
TAR3= dailyRMSMon
TAR4= dailyCoherenceMon
TAR5= genDailySummaryPage
TAR6= dailySpectrum
TAR7= dailySpectrogram
TAR8= dailyTimeSeries
#TAR9= dailyBruco
TAR10= dailyLT
TARs= ${TAR1} ${TAR2} ${TAR4} ${TAR6} ${TAR7} ${TAR8} ${TAR3} ${TAR5} ${TAR9} ${TAR10}

# compile rule
all: ${TARs}

${TAR1}: ${TAR1}.hs
	${MAKE} ${TAR1}.mk

${TAR2}: ${TAR2}.hs
	${MAKE} ${TAR2}.mk

${TAR3}: ${TAR3}.hs
	${MAKE} ${TAR3}.mk

${TAR4}: ${TAR4}.hs
	${MAKE} ${TAR4}.mk

${TAR5}: ${TAR5}.hs
	${MAKE} ${TAR5}.mk

${TAR6}: ${TAR6}.hs
	${MAKE} ${TAR6}.mk

${TAR7}: ${TAR7}.hs
	${MAKE} ${TAR7}.mk

${TAR8}: ${TAR8}.hs
	${MAKE} ${TAR8}.mk

${TAR9}: ${TAR9}.hs
	${MAKE} ${TAR9}.mk

${TAR10}: ${TAR10}.hs
	${MAKE} ${TAR10}.mk

clean:
	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi

cleanall: clean
	rm -f ${TARs}

