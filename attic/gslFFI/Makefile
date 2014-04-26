#******************************************#
#     File Name: Makefile
#        Author: Takahiro Yamamoto
# Last Modified: 2014/04/12 19:47:51
#******************************************#

# compiler option
CC = gcc -O2 -Wall -Wextra
HC = ghc -O2

# use library
USELIB=

# program
TAR1=testGSL
TAR= ${TAR1}

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

clean:
	rm -f ./*~ ./*.hi ./*.o ./WrapGSL/*~ ./WrapGSL/*.hi ./WrapGSL/*.o

cleanall: clean
	rm -f ${TAR}

