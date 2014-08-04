#makefile
#Time-stamp: "2014-08-03 17:36:19 yuzurihara"

CC=gcc

OBJ1=examplePurmutationTest.o ./HasKAL/StatisticsUtils/libStatisticsUtils.o
OBJ2=testPermutationTest.hs HasKAL/StatisticsUtils/Functions.hs HasKAL/StatisticsUtils/statisticsUtils.c

GSLLINK=-lgsl -lgslcblas
TARGET=examplePurmutationTest testPermutationTest

all:$(TARGET)

examplePurmutationTest:$(OBJ1)
	ln -fs ../../HasKAL/src/HasKAL ./
	$(CC) -o $@ $(OBJ1) $(GSLLINK)
	@echo "*** Making $@ ***"

testPermutationTest:$(OBJ2)
	ln -fs ../../HasKAL/src/HasKAL ./
	ghc --make ${OBJ2} -lgsl -lgslcblas
	@echo "*** Making $@ ***"
clean:
	rm -f *.o $(TARGET) *.hi
.cc.o:
	$(CC) -o $<




#-L/play/kagra/soft/gsl/lib
