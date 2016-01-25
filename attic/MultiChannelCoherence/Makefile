

# compiler option
CC = gcc -O2 -Wall -Wextra
HC = ghc -O2

# use library
USELIB=libframe

# program
TAR1=main
TARs= ${TAR1}

# link path
ifneq (${USELIB},)
CFLAGS=`pkg-config --cflags ${USELIB}`
LDFLAGS=`pkg-config --libs-only-L ${USELIB}`
LIBS=`pkg-config --libs-only-l ${USELIB}`
endif

DEP1=./HasKAL/PlotUtils/HROOT/AppendFunction.o
# DEP2=./HasKAL/SignalProcessingUtils/filterFunctions.o
DEP3=./HasKAL/LineUtils/LineRemoval/rng_median.o
DEPs= ${DEP1} ${DEP2} ${DEP3}

# compile rule
all: ${TARs}

${TAR1}: ${TAR1}.hs ${DEPs}
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

clean:
	rm -f ./*~ ./*.o ./*.hi

cleanall: clean
	rm -f ${TARs}

