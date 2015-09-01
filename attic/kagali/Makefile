PROGS = test

CFLAGS := -O2 -Wall -optc -std=c99

INCLUDEDIR = -I/home/ueno/kagali/include/kagali
LIBDIR = -L/home/ueno/kagali/lib

OBJS = test.hs

all:$(PROGS)

test: $(OBJS) DKGLInferenceSamplefn.c
	ghc $(CFLAGS) $(OBJS) DKGLInferenceSamplefn.c -lkagali $(LIBDIR) $(INCLUDEDIR)

clean:
	/bin/rm -f main_nha *.hi
