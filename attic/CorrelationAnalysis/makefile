CC=ghc
HC=ghc -O2

OBJ1=check_correlation.hs
OBJ3=generateUpconvNoise.hs
OBJ4=check_correlation_chunk.hs

DEP1 = ./HasKAL/PlotUtils/HROOT/AppendFunction.cc
DEP2 = ./HasKAL/LineUtils/LineRemoval/rng_median.c
DEP3 = ./HasKAL/SignalProcessingUtils/filterFunctions.c
LIBLINK=-L/opt/work/libframe-8.21/lib -L/opt/work/root-5.34.19/lib/root -lFrame -lm
INCLINK=-I/opt/work/libframe-8.21/include -I/opt/work/root-5.34.19/include/root 

HASKALPATH=../../HasKAL/src/HasKAL

#TARGET=check_correlation check_correlation_chunk
TARGET=check_correlation

all:$(TARGET)

generateUpconvNoise:${OBJ3}
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${DEP2}
	@echo "*** Making $@ ***"

check_correlation:${OBJ1} ${DEP3}
	ln -fs ${HASKALPATH} ./
	-ghc --make -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} -fPIC  -O2
	-ghc --make -o $@ $^ -lFrame -fPIC -lm -lgsl -lgslcblas  -O2
	@echo "*** Making $@ ***"

check_correlation_chunk:${OBJ4} ${DEP1} ${DEP2}
	ln -fs ${HASKALPATH} ./
	-ghc --make -o $@ check_correlation_chunk.hs ${CFLAGS} ${LDFLAGS} ${LIBS} -fPIC  -O2
	-ghc --make -o $@ check_correlation_chunk.hs ${DEP1} -lFrame -fPIC -lm -lgsl -lgslcblas  -O2
	@echo "*** Making $@ ***"


clean:
	rm -f *.o *.hi *.dyn_o *.dyn_hi
	rm -f  $(TARGET)


