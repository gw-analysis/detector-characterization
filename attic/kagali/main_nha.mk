HC = ghc -O2 -optc -std=c99

LIB = libframe kagali

CFLAGS=`pkg-config --cflags ${LIB}`
LDFLAGS=`pkg-config --libs-only-L ${LIB}`
LIBS=`pkg-config --libs-only-l ${LIB}`

CSRC = HasKAL/ExternalUtils/KAGALI/DKGLUtils.c 

all: main_nha_txt main_nha_xend main_nha_raw main_nha_ligo_16k main_nha_ligo_4k main_nha_kagra_1145854816 main_nha_kagra_1145855424 main_nha_schumann_magx

main_nha_txt: main_nha_txt.hs $(CSRC)
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}

main_nha_xend: main_nha_xend.hs $(CSRC)
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}

main_nha_raw: main_nha_raw.hs $(CSRC)
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}

main_nha_ligo_16k: main_nha_ligo_16k.hs $(CSRC)
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}

main_nha_ligo_4k: main_nha_ligo_4k.hs $(CSRC)
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}

main_nha_kagra_1145854816: main_nha_kagra_1145854816.hs $(CSRC)
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}

main_nha_kagra_1145855424: main_nha_kagra_1145855424.hs $(CSRC)
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}

main_nha_schumann_magx: main_nha_schumann_magx.hs $(CSRC)
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}

clean:
	/bin/rm -f main_nha_txt main_nha_xend main_nha_raw main_nha_ligo_16k main_nha_ligo_4k main_nha_kagra_1145854816 main_nha_kagra_1145855424 main_nha_schumann_magx *.hi *.o
