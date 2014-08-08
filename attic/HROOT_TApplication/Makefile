#******************************************#
#     File Name: Makefile
#        Author: Takahiro Yamamoto
# Last Modified: 2014/08/08 12:35:30
#******************************************#

# compiler option
HC = ghc -O2

# use library
USELIB = 

# program
TAR = plotTest

# link path
ifneq (${USELIB},)
CFLAGS=`pkg-config --cflags ${USELIB}`
LDFLAGS=`pkg-config --libs-only-L ${USELIB}`
LIBS=`pkg-config --libs-only-l ${USELIB}`
endif

# compile rule
all: ${TAR}

${TAR}: ${TAR}.hs
	ln -fs ../../HasKAL/src/HasKAL
	${HC} --make -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

clean:
	rm -f ./*~ ./*.o ./*.hi

cleanall: clean
	rm -f ${TAR} ./*.txt ./*.png ./HasKAL

