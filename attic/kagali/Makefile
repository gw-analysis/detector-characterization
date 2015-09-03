CFLAGS := -O2 -Wall -optc -std=c99

INCLUDEDIR = -I/home/ueno/kagali/include/kagali

LIBDIR = -L/home/ueno/kagali/lib

CSRC = DKGLInferenceSamplefn.c DKGLIterativeLeastSquare2DNewton.c

all: test main_nha

test: test.hs DKGLInferenceSamplefn.c
	ghc $(CFLAGS) test.hs $(CSRC) -lkagali $(LIBDIR) $(INCLUDEDIR)

main_nha: main_nha.hs DKGLIterativeLeastSquare2DNewton.c
	ghc $(CFLAGS) main_nha.hs $(CSRC) -lkagali $(LIBDIR) $(INCLUDEDIR)

clean:
	/bin/rm -f test main_nha *.hi
