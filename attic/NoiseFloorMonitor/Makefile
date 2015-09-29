#******************************************#
#     File Name: Makefile
#        Author: Takahiro Yamamoto
# Last Modified: 2014/06/24 11:47:31
#******************************************#

# compiler option
CC = gcc -O2 -Wall -Wextra
HC = ghc -O2

# use library
USELIB=

# program
TAR1=test_nfm_gwf
TAR2=test_nfm_wavedata
TAR3=test_wavedata
TAR4=check_wfilter
TAR= ${TAR1} ${TAR2} ${TAR3} ${TAR4}

# link path
ifneq (${USELIB},)
CFLAGS=`pkg-config --cflags ${USELIB}`
LDFLAGS=`pkg-config --libs-only-L ${USELIB}`
LIBS=`pkg-config --libs-only-l ${USELIB}`
else
CFLAGS=
LDFLAGS=
LIBS=-lgsl -lgslcblas -lstdc++ -lm -lFrame -L/opt/work/gsl/lib -I/opt/work/gsl/include -L/opt/work/root/lib -I/opt/work/root/include
##LIBS=-lgsl -lgslcblas -lstdc++ -lm -L/opt/work/gsl/lib -I/opt/work/gsl/include -L/opt/work/root/lib -I/opt/work/root/include  

endif

# compile rule
all: ${TAR}

${TAR1}: ${TAR1}.hs HasKAL/SignalProcessingUtils/filterFunctions.o
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} 

${TAR2}: ${TAR2}.hs HasKAL/SignalProcessingUtils/filterFunctions.o
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}
${TAR3}: ${TAR3}.hs HasKAL/SignalProcessingUtils/filterFunctions.o
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}
${TAR4}: ${TAR4}.hs HasKAL/SignalProcessingUtils/filterFunctions.o
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

clean:
	rm -f ./*~ ./*.hi ./*.o ./WrapGSL/*~ ./WrapGSL/*.hi ./WrapGSL/*.o

cleanall: clean
	rm -f ${TAR}

