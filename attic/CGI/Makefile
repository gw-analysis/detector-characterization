#******************************************#
#     File Name: Makefile
#        Author: Takahiro Yamamoto
# Last Modified: 2015/11/18 21:17:31
#******************************************#

# compiler option
CC = gcc -O2 -std=c99 -Wall -Wextra 
HC = ghc -O2 -optc -std=c99

# use library
USELIB=libframe kagali

# program
TAR1=date_1.cgi
TAR2=date_2.cgi
TAR3=date_3.cgi
TAR4=date_4.cgi
TAR5=date_5.cgi
TAR6=webToolFrame.cgi
TAR7=webToolPortal.cgi
TAR8=calSubSys.cgi
TAR9=dailyFrame.cgi
TAR10=dailySubSys.cgi
TAR11=generateChannelList.cgi
TAR12=selectChannelList.cgi
TARs= ${TAR11} ${TAR10} ${TAR9} ${TAR8} ${TAR7} ${TAR6} ${TAR5} ${TAR4} ${TAR3} ${TAR2} ${TAR1} ${TAR12}

# HasKAL 関連
DEP1=./HasKAL/PlotUtils/HROOT/AppendFunction.o
DEP2=./HasKAL/SignalProcessingUtils/filterFunctions.o
DEPs= ${DEP1} ${DEP2}

# kagali 関連
DEPK1=./HasKAL/ExternalUtils/KAGALI/DKGLUtils.o
DEPKs= ${DEPK1} ${DEPK2} ${DEPK3}

# link path
ifneq (${USELIB},)
CFLAGS=`pkg-config --cflags ${USELIB}`
LDFLAGS=`pkg-config --libs-only-L ${USELIB}`
LIBS=`pkg-config --libs-only-l ${USELIB}`
endif

# compile rule
all: ${TARs}

${TAR1}: ${TAR1}.hs ${DEPs} ${DEPKs}
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	chmod 755 ${TAR1}
	mv -f ${TAR1} ~/public_html/cgi-bin/

${TAR2}: ${TAR2}.hs ${DEPs}
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	chmod 755 ${TAR2}
	mv -f ${TAR2} ~/public_html/cgi-bin/

${TAR3}: ${TAR3}.hs ${DEP2}
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	chmod 755 ${TAR3}
	mv -f ${TAR3} ~/public_html/cgi-bin/

${TAR4}: ${TAR4}.hs ${DEP2}
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	chmod 755 ${TAR4}
	mv -f ${TAR4} ~/public_html/cgi-bin/

${TAR5}: ${TAR5}.hs ${DEPs}
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	chmod 755 ${TAR5}
	mv -f ${TAR5} ~/public_html/cgi-bin/

${TAR6}: ${TAR6}.hs
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	chmod 755 ${TAR6}
	mv -f ${TAR6} ~/public_html/cgi-bin/

${TAR7}: ${TAR7}.hs
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	chmod 755 ${TAR7}
	mv -f ${TAR7} ~/public_html/cgi-bin/

${TAR8}: ${TAR8}.hs
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	chmod 755 ${TAR8}
	mv -f ${TAR8} ~/public_html/cgi-bin/

${TAR9}: ${TAR9}.hs
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	chmod 755 ${TAR9}
	mv -f ${TAR9} ~/public_html/cgi-bin/

${TAR10}: ${TAR10}.hs
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	chmod 755 ${TAR10}
	mv -f ${TAR10} ~/public_html/cgi-bin/

${TAR11}: ${TAR11}.hs
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	chmod 755 ${TAR11}
	mv -f ${TAR11} ~/public_html/cgi-bin/

${TAR12}: ${TAR12}.hs
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	chmod 755 ${TAR12}
	mv -f ${TAR12} ~/public_html/cgi-bin/


clean:
	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi 

cleanall: clean
	rm -f ${TARs}

