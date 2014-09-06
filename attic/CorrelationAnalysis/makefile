#Time-stamp: "2014-09-07 04:27:56 yuzurihara"

CC=ghc
HC=ghc -O2

OBJ1=check_correlation.hs
OBJ2=generateHtml.hs HasKAL/MonitorUtils/CorrelationMon/CalCorrelation.hs
OBJ3=frameReadCheck.hs
OBJ4=frameData2SpectrogramePic.hs
OBJ5=frameData2CalibratedData.hs

DEP = ./HasKAL/PlotUtils/HROOT/SignalHandlerROOT.cc
LIBLINK=-L/opt/work/libframe-8.21/lib -L/opt/work/root-5.34.19/lib/root -lFrame -lm
INCLINK=-I/opt/work/libframe-8.21/include -I/opt/work/root-5.34.19/include/root 
#ROOTINC=-L/opt/work/root/include

#FRAMEDATA=~/detector-characterization/optFiles/sample-data/test-1066392016-300.gwf
HASKALPATH=~/detector-characterization/HasKAL/src/HasKAL

TARGET=check_correlation generateHtml frameReadCheck frameData2SpectrogramePic frameData2CalibratedData

all:$(TARGET)

frameData2CalibratedData:${OBJ5}
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${DEP} ${INCLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

frameData2SpectrogramePic:${OBJ4}
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${DEP} ${INCLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

frameReadCheck:${OBJ3}
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${DEP} ${INCLINK} ${LIBLINK}
	@echo "*** Making $@ ***"
#	$(CC) -O ${OBJ3} -o $@ ${INKLINK} ${LIBLINK} ${ROOTLIB} ${ROOTINC}


generateHtml:${OBJ2}
	ln -fs ${HASKALPATH} ./
	$(CC) -O ${OBJ2} -o $@ ${INKLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

check_correlation:${OBJ1}
	ln -fs ${HASKALPATH} ./
	$(CC) -O ${OBJ1} -o $@ ${INKLINK} ${LIBLINK}
	@echo "*** Making $@ ***"


testplot2:${OBJ2}
	ln -fs ${HASKALPATH} ./
	ln -fs ${FRAMEDATA} ./
	$(CC) -O ${OBJ2} -o $@ ${INKLINK} ${LIBLINK}
	@echo "*** Making $@ ***"


testplot3:${OBJ3}
	ln -fs ${HASKALPATH} ./
	$(CC) -O ${OBJ3} -o $@ ${INKLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

clean:
	rm -f *.o *.hi
	rm -f  $(TARGET)


