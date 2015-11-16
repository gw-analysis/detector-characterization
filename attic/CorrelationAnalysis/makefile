CC=ghc
HC=ghc -O2

OBJ1=check_correlation.hs
OBJ2=generateHtml.hs HasKAL/MonitorUtils/CorrelationMon/CalCorrelation.hs
OBJ15=generateUpconvNoise.hs

#DEP1 = ./HasKAL/PlotUtils/HROOT/AppendFunction.cc
LIBLINK=-L/opt/work/libframe-8.21/lib -L/opt/work/root-5.34.19/lib/root -lFrame -lm
INCLINK=-I/opt/work/libframe-8.21/include -I/opt/work/root-5.34.19/include/root 

HASKALPATH=../../HasKAL/src/HasKAL

TARGET=check_correlation

all:$(TARGET)

generateUpconvNoise:${OBJ15}
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${DEP2}
	@echo "*** Making $@ ***"

check_correlation:${OBJ1} ${DEP1}
	ln -fs ${HASKALPATH} ./
	-ghc --make -o $@ check_correlation.hs ${CFLAGS} ${LDFLAGS} ${LIBS} -fPIC  -O2
	-ghc --make -o $@ check_correlation.hs ${DEP1} -lFrame -fPIC -lm -lgsl -lgslcblas  -O2
	@echo "*** Making $@ ***"


clean:
	rm -f *.o *.hi
	rm -f  $(TARGET)


