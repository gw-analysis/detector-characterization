HC = ghc -O2 -optc -std=c99

LIB = libframe kagali

CFLAGS=`pkg-config --cflags ${LIB}`
LDFLAGS=`pkg-config --libs-only-L ${LIB}`
LIBS=`pkg-config --libs-only-l ${LIB}`

CSRC = DKGLUtils.c

all: main_nha_txt main_nha_frame

main_nha_txt: main_nha_txt.hs $(CSRC)
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}

main_nha_frame: main_nha_frame.hs $(CSRC)
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} ${CFLAGS2}

clean:
	/bin/rm -f main_nha_txt main_nha_frame *.hi *.o
