#******************************************#
#     File Name: genDailyCmd.mk
#        Author: Takahiro Yamamoto
# Last Modified: 2015/11/13 20:53:11
#******************************************#

# compiler option
HC = ghc -O2

# use library
USELIB=

# program
TAR1= genDailyCmd
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

