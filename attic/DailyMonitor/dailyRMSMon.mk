# dailyRMSMon.mk

HC=ghc --make

OBJ1=dailyRMSMon.hs

DEP1 = ./HasKAL/PlotUtils/HROOT/AppendFunction.cc
LIBLINK= -lFrame -lm -lgsl -lgslcblas

HASKALPATH=../../HasKAL/src/HasKAL

TARGET=sypoliclink dailyRMSMon

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

dailyRMSMon:${OBJ1} ${DEP1}
	-ghc --make -o dailyRMSMon dailyRMSMon.hs ${CFLAGS} ${LDFLAGS} ${LIBS}-fPIC
	-ghc --make -o dailyRMSMon dailyRMSMon.hs ${DEP1} -lFrame -fPIC -lm -lgsl -lgslcblas
	@echo "*** Making $@ ***"


clean:
	rm -f *.o *.hi
	rm -f $(TARGET)
	rm -f HasKAL
	rm -f *.dyn_hi *.dyn_o

