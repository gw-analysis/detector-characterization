HC = ghc -O2 -optc -std=c99

LIB = libframe kagali

CFLAGS=`pkg-config --cflags ${LIB}`
LDFLAGS=`pkg-config --libs-only-L ${LIB}`
LIBS=`pkg-config --libs-only-l ${LIB}`

CSRC = HasKAL/ExternalUtils/KAGALI/DKGLUtils.c

all: main_nha_txt main_nha_xend main_nha_raw

main_nha_txt: main_nha_txt.hs $(CSRC)
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}

main_nha_xend: main_nha_xend.hs $(CSRC)
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}

main_nha_raw: main_nha_raw.hs $(CSRC)
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}

clean:
	/bin/rm -f main_nha_txt main_nha_xend main_nha_raw *.hi *.o
