#makefile
#Time-stamp: "2014-07-28 17:09:27 yuzurihara"

CC=gcc

OBJ1=examplePurmutationTest.o ./HasKAL/StatisticsUtils/libStatisticsUtils.o
OBJ2=

GSLLINK=-lgsl -lgslcblas
TARGET=examplePurmutationTest

all:$(TARGET)

examplePurmutationTest:$(OBJ1)
	ln -fs ../../HasKAL/src/HasKAL ./
	$(CC) -o $@ $(OBJ1) $(GSLLINK)
	@echo "*** Making $@ ***"

:$(OBJ2)
	$(CC) -o $@ $(OBJ2)
	@echo "*** Making $@ ***"
clean:
	rm -f *.o $(TARGET) *.hi
.cc.o:
	$(CC) -o $<




