#******************************************#
#     File Name: Makefile
#        Author: Takahiro Yamamoto
# Last Modified: 2015/11/03 00:59:29
#******************************************#

MAKE=make -f

# program
TAR1= dailySRMon
TAR2= dailyRMon
TAR3= dailyRMSMon
TAR4= dailyCoherenceMon
TAR5= genDailySummaryPage
TARs= ${TAR1} ${TAR2} ${TAR4} ${TAR3} ${TAR5}

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


clean:
	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi

cleanall: clean
	rm -f ${TARs}

