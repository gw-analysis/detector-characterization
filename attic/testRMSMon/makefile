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

TARGET=sypoliclink testRMSMon testDailyRMSMon

all:$(TARGET)

sypoliclink:
	ln -fs ${HASKALPATH} ./

testRMSMon:${OBJ1} ${DEP3}
	${HC} -o $@ $^ -I/home/detchar/tools/include/root -lFrame -fPIC -lm -lgsl -lgslcblas
	@echo "*** Making $@ ***"

testDailyRMSMon:${OBJ1} ${DEP3}
	${HC} -o $@ $^ -I/home/detchar/tools/include/root -lFrame -fPIC -lm -lgsl -lgslcblas
	@echo "*** Making $@ ***"



clean:
	rm -f *.o *.hi
	rm -f  $(TARGET)
	rm -f HasKAL

