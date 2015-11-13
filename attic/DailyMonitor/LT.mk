# compiler option
HC = ghc -O2 -optc -std=c99

# use library
USELIB= libframe kagali

# program
TAR1= dailyLT
TARs= ${TAR1}

# dependency
DEP1= ./HasKAL/PlotUtils/HROOT/AppendFunction.cc
CSRC = ./HasKAL/ExternalUtils/KAGALI/DKGLUtils.c
DEPs= ${DEP1}

#########################################
# link path
ifneq (${USELIB},)
CFLAGS=`pkg-config --cflags ${USELIB}`
LDFLAGS=`pkg-config --libs-only-L ${USELIB}`
LIBS=`pkg-config --libs-only-l ${USELIB}`
endif

# compile rule
all: ${TARs}

${TAR1}: ${TAR1}.hs ${DEPs} ${CSRC}
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

clean:
	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi

cleanall: clean
	rm -f ${TARs}

