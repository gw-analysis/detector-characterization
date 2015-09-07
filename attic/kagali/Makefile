CFLAGS := -O2 -Wall -optc -std=c99

INCLUDEDIR = -I/home/ueno/kagali/include/kagali

LIBDIR = -L/home/ueno/kagali/lib

CSRC = DKGLInferenceSamplefn.c DKGLBandPassFilter.c DKGLIterativeLeastSquare2DNewton.c

all: test main_nha_txt main_nha_frame

test: test.hs DKGLInferenceSamplefn.c
	ghc $(CFLAGS) test.hs $(CSRC) -lkagali $(LIBDIR) $(INCLUDEDIR)

main_nha_txt: main_nha_txt.hs DKGLBandPassFilter.c DKGLIterativeLeastSquare2DNewton.c
	ghc $(CFLAGS) main_nha_txt.hs $(CSRC) -lkagali $(LIBDIR) $(INCLUDEDIR)

main_nha_frame: main_nha_frame.hs DKGLBandPassFilter.c DKGLIterativeLeastSquare2DNewton.c
	ghc $(CFLAGS) main_nha_frame.hs $(CSRC) -lkagali $(LIBDIR) $(INCLUDEDIR)

clean:
	/bin/rm -f test main_nha_txt main_nha_frame *.hi
