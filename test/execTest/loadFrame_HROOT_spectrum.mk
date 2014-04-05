#******************************************#
#     File Name: loadFrame_HROOT_spectrum.mk
#        Author: Takahiro Yamamoto
# Last Modified: 2014/03/18 13:21:52
#******************************************#

# compiler option
HC = ghc -O
HSC = hsc2hs

CMD_LN := ln

# use library
USELIB=fftw3 libframe

# program
TAR1=loadFrame_HROOT_spectrum
SRC1=./HasKAL/FrameUtils/FrameUtils.hs
TAR= ${TAR1}

# link path
ifneq (${USELIB},)
CFLAGS=`pkg-config --cflags ${USELIB}`
LDFLAGS=`pkg-config --libs-only-L ${USELIB}`
LIBS=`pkg-config --libs-only-l ${USELIB}`
endif

# compile rule
all: ${TAR}

${TAR1}: ${TAR1}.hs ${SRC1}
	${HC} -o $@ $< ${CFLAGS} ${LDFLAGS} ${LIBS}
	${CMD_LN} -s ../sample-data/test-1066392016-300.gwf ./test-1066392016-300.gwf

${SRC1}: ${SRC1}c
	${HSC} -o $@ $^ 

clean:
	rm -f ./*~

cleanall:
	rm -f ./*~ ./*.hi ./*.o ./HasKAL/FrameUtils/*.hi ./HasKAL/FrameUtils/*.o ${TAR} ${SRC1} ./test-1066392016-300.gwf

