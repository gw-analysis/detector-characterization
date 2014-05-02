#Time-stamp: "2013-11-12 14:53:15 yuzurihara"

CC=ghc

OBJ1=testplot.hs HasKAL/PlotUtils/PlotUtilsHROOT.hs
OBJ2=testplot2.hs HasKAL/PlotUtils/PlotUtilsHROOT.hs

#INCLINK=-I/home/yuzu/src/libframe-8.20/include/
#LIBLINK=-lstdc++ -lFrame -L/home/yuzu/src/libframe-8.20/include/
LIBLINK=-lstdc++ -lm -lFrame

#TARGET=testplot
TARGET=testplot testplot2

all:$(TARGET)

testplot:${OBJ1}
	$(CC) -O ${OBJ1} -o $@ ${INKLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

testplot2:${OBJ2}
	$(CC) -O ${OBJ2} -o $@ ${INKLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

clean:
	rm -f *.o *.hi
	rm -f  $(TARGET)


