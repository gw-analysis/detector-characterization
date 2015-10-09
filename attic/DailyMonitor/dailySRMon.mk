#******************************************#
#     File Name: dailySRMon.mk
#        Author: Takahiro Yamamoto
# Last Modified: 2015/10/09 11:33:18
#******************************************#

# compiler option
HC = ghc -O2

# use library
USELIB= libframe

# program
TAR1= dailySRMon
TAR2= dailySRMon_reduceMemory
TARs= ${TAR1} ${TAR2}

# dependency
DEP1= ./HasKAL/PlotUtils/HROOT/AppendFunction.cc
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

${TAR1}: ${TAR1}.hs ${DEPs}
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

${TAR2}: ${TAR2}.hs ${DEPs}
	-${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}


clean:
	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi

cleanall: clean
	rm -f ${TARs}

