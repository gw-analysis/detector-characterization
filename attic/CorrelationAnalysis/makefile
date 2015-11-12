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
OBJ14=outputCorrelatedData2Text.hs 
OBJ15=generateUpconvNoise.hs


#DEP1 = ./HasKAL/PlotUtils/HROOT/AppendFunction.cc
LIBLINK=-L/opt/work/libframe-8.21/lib -L/opt/work/root-5.34.19/lib/root -lFrame -lm
INCLINK=-I/opt/work/libframe-8.21/include -I/opt/work/root-5.34.19/include/root 

HASKALPATH=../..//HasKAL/src/HasKAL

TARGET=check_correlation

all:$(TARGET)

generateUpconvNoise:${OBJ15}
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${DEP2}
	@echo "*** Making $@ ***"


outputCorrelateRawData2Text: outputCorrelateRawData2Text.hs 
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${DEP2} ${INCLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

outputCorrelatedData2Text:${OBJ14}
	ln -fs ${HASKALPATH} ./
	${HC} --make -o $@ $^ ${DEP2} ${INCLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

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
	${HC} --make -o $@ $^ ${DEP2} ${DEP} ${INCLINK} ${LIBLINK}
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

check_correlation:${OBJ1} ${DEP1}
	ln -fs ${HASKALPATH} ./
	-ghc --make -o $@ check_correlation.hs ${CFLAGS} ${LDFLAGS} ${LIBS} -fPIC  -O2
	-ghc --make -o $@ check_correlation.hs ${DEP1} -lFrame -fPIC -lm -lgsl -lgslcblas  -O2
	@echo "*** Making $@ ***"


clean:
	rm -f *.o *.hi
	rm -f  $(TARGET)


