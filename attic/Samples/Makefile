#******************************************#
#     File Name: Makefile
#        Author: Takahiro Yamamoto
# Last Modified: 2015/06/09 14:55:26
#******************************************#

# compiler option
CC = gcc -O2 -Wall -Wextra
HC = ghc -O2

# use library
USELIB= 

# program
TAR1= plotTimeSeriesFromFrame
TAR2= plotSpectrumFromFrame
TAR3= plotSpectrogramFromFrame
TARs= ${TAR1} ${TAR2} ${TAR3}

DEP= ./HasKAL/PlotUtils/HROOT/AppendFunction.cc

# link path
ifneq (${USELIB},)
CFLAGS=`pkg-config --cflags ${USELIB}`
LDFLAGS=`pkg-config --libs-only-L ${USELIB}`
LIBS=`pkg-config --libs-only-l ${USELIB}`
endif

# compile rule
all: ${TARs}

${TAR1}: ${TAR1}.hs ${DEP}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} -L/home/detchar/tools/lib -lFrame

${TAR2}: ${TAR2}.hs ${DEP}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} -L/home/detchar/tools/lib -lFrame

${TAR3}: ${TAR3}.hs ${DEP} 
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} -L/home/detchar/tools/lib -lFrame

clean:
	rm -f ./*~ ./*.o ./*.hi

cleanall: clean
	rm -f ${TARs}

