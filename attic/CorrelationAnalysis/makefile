#Time-stamp: "2014-09-16 06:06:41 yuzurihara"

CC=ghc
HC=ghc -O2

OBJ1=check_correlation.hs
OBJ2=generateHtml.hs HasKAL/MonitorUtils/CorrelationMon/CalCorrelation.hs
OBJ3=frameReadCheck.hs
OBJ4=frameData2SpectrogramePic.hs
OBJ5=frameData2CalibratedData.hs
OBJ6=frameData2PassFiltered.hs
OBJ7=spectrogram_check.hs
OBJ8=filter_check_lpf.hs
OBJ9=frameData2LPF_correlation.hs
OBJ10=devidedData2Correlation.hs
OBJ11=check_spect0908.hs
OBJ12=check_spect09082.hs
OBJ13=check_spect0910.hs



DEP = ./HasKAL/PlotUtils/HROOT/SignalHandlerROOT.cc
DEP2 = ./HasKAL/SignalProcessingUtils/filterFunctions.c
LIBLINK=-L/opt/work/libframe-8.21/lib -L/opt/work/root-5.34.19/lib/root -lFrame -lm
INCLINK=-I/opt/work/libframe-8.21/include -I/opt/work/root-5.34.19/include/root 
#ROOTINC=-L/opt/work/root/include

#FRAMEDATA=~/detector-characterization/optFiles/sample-data/test-1066392016-300.gwf
HASKALPATH=~/detector-characterization/HasKAL/src/HasKAL

#TARGET=check_correlation generateHtml frameReadCheck frameData2SpectrogramePic frameData2CalibratedData frameData2PassFiltered spectrogram_check filter_check_lpf
TARGET=frameData2LPF_correlation check_spect0908 check_spect09082 check_spect0910

all:$(TARGET)

check_spect0910:${OBJ13}
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${DEP2} ${INCLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

check_spect09082:${OBJ12}
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${DEP2} ${INCLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

check_spect0908:${OBJ11}
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${DEP2} ${INCLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

devidedData2Correlation:${OBJ10}
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${DEP2} ${INCLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

frameData2LPF_correlation:${OBJ9}
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${DEP2} ${INCLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

filter_check_lpf:${OBJ8}
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${DEP2} ${INCLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

spectrogram_check:${OBJ7}
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${INCLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

frameData2PassFiltered:${OBJ6}
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${DEP2} ${INCLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

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


