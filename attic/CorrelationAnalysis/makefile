#Time-stamp: "2014-08-04 15:47:17 yuzurihara"

# To compile, you must do follow command before make command
#	cd ~/detector-characterization/HasKAL
#	cabal install --only-dependencies
#	cabal configure
#	cabal build

CC=ghc
HC=ghc -O2

OBJ1=check_correlation.hs
OBJ2=generateHtml.hs HasKAL/MonitorUtils/CorrelationMon/CalCorrelation.hs
OBJ3=frameReadCheck.hs

#DEP = ./HasKAL/PlotUtils/HROOT/SignalHandlerROOT.cc
LIBLINK=-L/opt/work/libframe-8.21/lib -L/opt/work/root-5.34.19/lib/root -lFrame -lm
INCLINK=-I/opt/work/libframe-8.21/include -I/opt/work/root-5.34.19/include/root 
#ROOTINC=-L/opt/work/root/include

#FRAMEDATA=~/detector-characterization/optFiles/sample-data/test-1066392016-300.gwf
HASKALPATH=~/detector-characterization/HasKAL/src/HasKAL

TARGET=check_correlation generateHtml frameReadCheck

all:$(TARGET)
frameReadCheck:${OBJ3}
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${INCLINK} ${LIBLINK}
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


