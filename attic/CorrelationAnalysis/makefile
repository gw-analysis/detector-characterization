#Time-stamp: "2013-11-12 14:53:15 yuzurihara"

# To compile, you must do follow command before make command
#	cd ~/detector-characterization/HasKAL
#	cabal install --only-dependencies
#	cabal configure
#	cabal build

CC=ghc

OBJ1=check_correlation.hs
OBJ2=
OBJ3=


#INCLINK=-I/home/yuzu/src/libframe-8.20/include/
#LIBLINK=-lstdc++ -lFrame -L/home/yuzu/src/libframe-8.20/include/
LIBLINK=-lstdc++ -lm -lFrame

FRAMEDATA=~/detector-characterization/optFiles/sample-data/test-1066392016-300.gwf
HASKALPATH=~/detector-characterization/HasKAL/src/HasKAL

#TARGET=testplot1
#TARGET=testplot1 testplot2 testplot3
TARGET=check_correlation

all:$(TARGET)

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


