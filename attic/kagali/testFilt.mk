#******************************************#
#     File Name: Makefile
#        Author: Takahiro Yamamoto
# Last Modified: 2015/09/14 13:22:57
#******************************************#

# compiler option
CC = gcc -O2 -Wall -Wextra 
HC = ghc -O2 -optc -std=c99 

# use library
USELIB=libframe kagali

# program
TAR1=testFilt
TARs= ${TAR1}

# HasKAL 関連
DEP1=./HasKAL/PlotUtils/HROOT/AppendFunction.cc
DEPs= ${DEP1}

# kagali 関連
CFLAGS2=-I./
DEPK1=/home/yamamoto/work/detector-characterization/attic/kagali/DKGLInferenceSamplefn.c
DEPK2=/home/yamamoto/work/detector-characterization/attic/kagali/DKGLIterativeLeastSquare2DNewton.c
DEPK3=/home/yamamoto/work/detector-characterization/attic/kagali/DKGLBandPassFilter.c
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
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}

clean:
	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi 

cleanall: clean
	rm -f ${TARs}

