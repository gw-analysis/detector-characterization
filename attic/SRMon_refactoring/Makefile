#******************************************#
#     File Name: Makefile
#        Author: Takahiro Yamamoto
# Last Modified: 2015/02/20 15:53:48
#******************************************#

# compiler option
CC = gcc -O2 -Wall -Wextra
HC = ghc -O2

# use library
USELIB=libframe gsl

# program
TAR1= testRUN007
TARs= ${TAR1}
DEPs= ./HasKAL/PlotUtils/HROOT/AppendFunction.cc \
      ./HasKAL/SignalProcessingUtils/filterFunctions.c

# link path
ifneq (${USELIB},)
CFLAGS=`pkg-config --cflags ${USELIB}`
LDFLAGS=`pkg-config --libs-only-L ${USELIB}`
LIBS=`pkg-config --libs-only-l ${USELIB}`
endif

# compile rule
all: ${TARs}

${TAR1}: ${TAR1}.hs ${DEPs}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

clean:
	rm -f ./*~ ./*.o ./*.hi

cleanall: clean
	rm -f ${TARs}

