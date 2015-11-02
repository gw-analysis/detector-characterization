#******************************************#
#     File Name: Makefile
#        Author: Takahiro Yamamoto
# Last Modified: 2015/09/17 17:29:37
#******************************************#

# compiler option
CC = gcc -O2 -Wall -Wextra 
HC = ghc -O2 -optc -std=c99

# use library
USELIB=libframe kagali

# program
TAR1=date_1.cgi
TAR2=main_1.cgi
#TAR3=date_2.cgi
#TAR4=main_2.cgi
#TAR5=date_3.cgi
#TAR6=main_3.cgi
#TAR7=date_4.cgi
#TAR8=main_4.cgi
TAR9=date_5.cgi
TAR10=main_5.cgi
#TAR11=cal.cgi
TARs= ${TAR11} ${TAR10} ${TAR9} ${TAR8} ${TAR7} ${TAR6} ${TAR5} ${TAR4} ${TAR3} ${TAR2} ${TAR1}

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
	mv -f ${TAR1} ~/public_html/cgi-bin/
#clean:
	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi 

${TAR2}: ${TAR2}.hs ${DEPs} ${DEPKs}
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	mv -f ${TAR2} ~/public_html/cgi-bin/
#clean:
	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi 

#${TAR3}: ${TAR3}.hs ${DEPs} ${DEPKs}
#	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
#	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
#	mv -f ${TAR3} ~/public_html/cgi-bin/
##clean:
#	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi 
#
#${TAR4}: ${TAR4}.hs ${DEPs} ${DEPKs}
#	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
#	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
#	mv -f ${TAR4} ~/public_html/cgi-bin/
##clean:
#	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi 
#
#${TAR5}: ${TAR5}.hs ${DEP2} ${DEPKs}
#	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
#	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
#	mv -f ${TAR5} ~/public_html/cgi-bin/
##clean:
#	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi 
#
#${TAR6}: ${TAR6}.hs ${DEP2} ${DEPKs}
#	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
#	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
#	mv -f ${TAR6} ~/public_html/cgi-bin/
##clean:
#	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi 
#
#${TAR7}: ${TAR7}.hs ${DEP2} ${DEPKs}
#	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
#	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
#	mv -f ${TAR7} ~/public_html/cgi-bin/
##clean:
#	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi 
#
#${TAR8}: ${TAR8}.hs ${DEP2} ${DEPKs}
#	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
#	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
#	mv -f ${TAR8} ~/public_html/cgi-bin/
##clean:
#	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi 
#
${TAR9}: ${TAR9}.hs ${DEPs} ${DEPKs}
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	mv -f ${TAR9} ~/public_html/cgi-bin/
#clean:
	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi 

${TAR10}: ${TAR10}.hs ${DEPs} ${DEPKs}
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
	mv -f ${TAR10} ~/public_html/cgi-bin/
#clean:
	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi 

#${TAR11}: ${TAR11}.hs ${DEP2} ${DEPKs}
#	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
#	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}
#	mv -f ${TAR11} ~/public_html/cgi-bin/
##clean:
#	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi 


#clean:
	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi 

#cleanall: clean
	rm -f ${TARs}

