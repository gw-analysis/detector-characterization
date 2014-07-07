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
TAR1=teststoch
#TAR2=gslRandomTest
#TAR3=gslHasKALTest
TAR= ${TAR1} ${TAR2} ${TAR3}

# link path
ifneq (${USELIB},)
CFLAGS=`pkg-config --cflags ${USELIB}`
LDFLAGS=`pkg-config --libs-only-L ${USELIB}`
LIBS=`pkg-config --libs-only-l ${USELIB}`
else
CFLAGS=
LDFLAGS=
LIBS=-lgsl -lgslcblas
endif

# compile rule
all: ${TAR}

${TAR1}: ${TAR1}.hs
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

${TAR2}: ${TAR2}.hs
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}
${TAR3}: ${TAR3}.hs
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

clean:
	rm -f ./*~ ./*.hi ./*.o ./WrapGSL/*~ ./WrapGSL/*.hi ./WrapGSL/*.o

cleanall: clean
	rm -f ${TAR}

