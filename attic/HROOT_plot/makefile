#Time-stamp: "2013-11-12 14:53:15 yuzurihara"

# my library
# CosmicString.o z2dlib.o fftwlib.o pos2flib.o event_genlib.o Matrixlib.o
# fftwlib.o noise_generatelib.o
CC=ghc

OBJ1=loadFrameData.hs
OBJ2=loadFrame_HROOT.hs
OBJ3=loadFrame_HROOT_spectrum.hs
OBJ4=loadFrame_HROOT_adc.hs
OBJ5=
OBJ6=
OBJ7=
OBJ8=
OBJ9=
OBJ10=

INCLINK=-I/home/yuzu/src/libframe-8.20/include/
LIBLINK=-lstdc++ -lFrame -L/home/yuzu/src/libframe-8.20/include/


#TARGET=loadFrame_HROOT loadFrameData
TARGET=loadFrame_HROOT_spectrum
#TARGET=loadFrameData
all:$(TARGET)

loadFrameData:$(OBJ1)
	$(CC) -O ${OBJ1} -o $@ ${INKLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

loadFrame_HROOT:${OBJ2}
	$(CC) -O ${OBJ2} -o $@ ${INKLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

loadFrame_HROOT_spectrum:${OBJ3}
	$(CC) -O ${OBJ3} -o $@ ${INKLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

loadFrame_HROOT_adc:${OBJ4}
	$(CC) -O ${OBJ4} -o $@ ${INKLINK} ${LIBLINK}
	@echo "*** Making $@ ***"

clean:
	rm -f *.o *.hi
	rm -f  $(TARGET)

.c.o:
	$(CC) -c $<
