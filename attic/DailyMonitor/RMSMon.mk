# dailyRMSMon.mk

HC=ghc --make

OBJ1=RMSMon.hs

DEP1 = ./HasKAL/PlotUtils/HROOT/AppendFunction.cc
DEP2= ./HasKAL/LineUtils/LineRemoval/rng_median.c
LIBLINK= -lFrame -lm -lgsl -lgslcblas

HASKALPATH=../../HasKAL/src/HasKAL

TARGET=sypoliclink RMSMon

#########################################
# link path
ifneq (${USELIB},)
CFLAGS=`pkg-config --cflags ${USELIB}`
LDFLAGS=`pkg-config --libs-only-L ${USELIB}`
LIBS=`pkg-config --libs-only-l ${USELIB}`
endif

all:$(TARGET)

sypoliclink:
	ln -fs ${HASKALPATH} ./

RMSMon:${OBJ1} ${DEP1} ${DEP2}
	-ghc --make -o $@ RMSMon.hs ${CFLAGS} ${LDFLAGS} ${LIBS} -fPIC  -O2
	-ghc --make -o $@ RMSMon.hs ${DEP1} ${DEP2} -lFrame -fPIC -lm -lgsl -lgslcblas  -O2
	@echo "*** Making $@ ***"

clean:
	rm -f *.o *.hi
	rm -f $(TARGET)
	rm -f HasKAL
	rm -f *.dyn_hi *.dyn_o

