#Time-stamp: "2013-11-12 14:53:15 yuzurihara"

# my library
# CosmicString.o z2dlib.o fftwlib.o pos2flib.o event_genlib.o Matrixlib.o
# fftwlib.o noise_generatelib.o
CC=ghc

OBJ1=testplot.hs
#OBJ1=testplot.hs PlotUtilsHROOT.hs
OBJ2=testplot2.hs

OBJ7=
OBJ8=
OBJ9=
OBJ10=

#INCLINK=-I/home/yuzu/src/libframe-8.20/include/
#LIBLINK=-lstdc++ -lFrame -L/home/yuzu/src/libframe-8.20/include/
LIBLINK=-lstdc++ -lm -lFrame

#TARGET=testplot
TARGET=testplot

all:$(TARGET)

testplot:${OBJ1}
	$(CC) -O ${OBJ1} -o $@ ${INKLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

testplot2:${OBJ2}
	$(CC) -O ${OBJ2} -o $@ ${INKLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

test_plot3:${OBJ7}
	$(CC) -O ${OBJ7} -o $@ ${INKLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

clean:
	rm -f *.o *.hi
	rm -f  $(TARGET)


