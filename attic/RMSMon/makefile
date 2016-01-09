# dailyRMSMon.mk

HC=ghc --make

OBJ1=RMSMon.hs
OBJ2=readTest.hs
OBJ3=checkGPS.hs

DEP1 = ./HasKAL/PlotUtils/HROOT/AppendFunction.cc
DEP2 = ./HasKAL/LineUtils/LineRemoval/rng_median.c
LIBLINK= -lFrame -lm -lgsl -lgslcblas

HASKALPATH=../../HasKAL/src/HasKAL

TARGET=sypoliclink RMSMon readTest

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

checkGPS:${OBJ3} ${DEP1} ${DEP2}
	-ghc --make -o $@ ${OBJ3} ${CFLAGS} ${LDFLAGS} ${LIBS} -fPIC  -O2
	-ghc --make -o $@ ${OBJ3} ${DEP1} ${DEP2} -lFrame -fPIC -lm -lgsl -lgslcblas  -O2
	@echo "*** Making $@ ***"

readTest:${OBJ2} ${DEP1} ${DEP2}
	-ghc --make -o $@ ${OBJ2} ${CFLAGS} ${LDFLAGS} ${LIBS} -fPIC  -O2
	-ghc --make -o $@ ${OBJ2} ${DEP1} ${DEP2} -lFrame -fPIC -lm -lgsl -lgslcblas  -O2
	@echo "*** Making $@ ***"

RMSMon:${OBJ1} ${DEP1} ${DEP2}
	-ghc --make -o $@ ${OBJ1} ${CFLAGS} ${LDFLAGS} ${LIBS} -fPIC  -O2
	-ghc --make -o $@ ${OBJ1} ${DEP1} ${DEP2} -lFrame -fPIC -lm -lgsl -lgslcblas  -O2
	@echo "*** Making $@ ***"

clean:
	rm -f *.o *.hi
	rm -f $(TARGET)
	rm -f HasKAL
	rm -f *.dyn_hi *.dyn_o

