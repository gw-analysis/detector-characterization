#******************************************#
#     File Name: Makefile
#        Author: Takahiro Yamamoto
# Last Modified: 2014/12/05 16:49:45
#******************************************#

# compiler option
CC = gcc -O2 -Wall -Wextra
HC = ghc -O2

# use library
USELIB=libframe

# program
TAR1= test
TARs= ${TAR1}

# link path
ifneq (${USELIB},)
CFLAGS=`pkg-config --cflags ${USELIB}`
LDFLAGS=`pkg-config --libs-only-L ${USELIB}`
LIBS=`pkg-config --libs-only-l ${USELIB}`
endif

# compile rule
all: ${TARs}

${TAR1}: ${TAR1}.hs
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

clean:
	rm -f ./*~ ./*.o ./*.hi

cleanall: clean
	rm -f ${TARs}

