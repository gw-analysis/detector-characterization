CC=ghc
HC=ghc --make

OBJ1=testRMSMon.hs

DEP = ./HasKAL/PlotUtils/HROOT/SignalHandlerHROOT.cc \
      ./HasKAL/PlotUtils/HROOT/SetRangeROOT.cc
DEP2 = ./HasKAL/SignalProcessingUtils/filterFunctions.c
DEP3 = ./HasKAL/PlotUtils/HROOT/AppendFunction.cc
LIBLINK= -lFrame -lm -lgsl -lgslcblas
INCLINK=-

HASKALPATH=../../HasKAL/src/HasKAL

TARGET=testRMSMon

all:$(TARGET)

testRMSMon:${OBJ1} 
	ln -fs ${HASKALPATH} ./
	${HC} -o $@ $^ ${DEP3} -lFrame -fPIC
	@echo "*** Making $@ ***"



clean:
	rm -f *.o *.hi
	rm -f  $(TARGET)
	rm -f HasKAL

