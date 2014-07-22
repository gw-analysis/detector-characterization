#******************************************#
#     File Name: Makefile
#        Author: Takahiro Yamamoto
# Last Modified: 2014/07/15 16:50:33
#******************************************#

# compiler option
HC = ghc -O2

# use library
USELIB = gsl libframe

# program
TAR2 = cuiSrm
TAR= ${TAR2}

DEPPATH = ./HasKAL/MonitorUtils/SRMon

# link path
ifneq (${USELIB},)
CFLAGS=`pkg-config --cflags ${USELIB}`
LDFLAGS=`pkg-config --libs-only-L ${USELIB}`
LIBS=`pkg-config --libs-only-l ${USELIB}`
endif

# compile rule
all: ${TAR}

${TAR2}: ${TAR2}.hs
	${HC} --make -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

clean:
	rm -f ./*~ ./*.o ./*.hi

cleanall: clean
	rm -f ${TAR} ./*.txt

