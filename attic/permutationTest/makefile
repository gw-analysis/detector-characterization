#makefile
#Time-stamp: "2014-08-04 15:16:38 yuzurihara"

PATH_STATISTICS=HasKAL/StatisticsUtils

OBJ1=examplePurmutationTest.o  libstatisticsUtils.a
OBJ2=testPermutationTest.hs  $(PATH_STATISTICS)/Functions.hs  libstatisticsUtils.a
OBJ3=statisticsUtils


GSLLINK=-lgsl -lgslcblas
TARGET=libstatisticsUtils.a examplePurmutationTest testPermutationTest


all:$(TARGET)

examplePurmutationTest:$(OBJ1)
	ln -fs ../../HasKAL/src/HasKAL ./
	gcc -o $@ $(OBJ1) $(GSLLINK)
	@echo "*** Making $@ ***"
	@echo ""


testPermutationTest:$(OBJ2)
	ln -fs ../../HasKAL/src/HasKAL ./
	ghc --make ${OBJ2} -lgsl -lgslcblas
	@echo "*** Making $@ ***"
	@echo ""

#- gcc -c statisticsUtils.c
#- ar r libstatisticsUtils.a statisticsUtils.o

libstatisticsUtils.a:$(PATH_STATISTICS)/$(OBJ3).c
	ln -fs ../../HasKAL/src/HasKAL ./
	gcc -c $(PATH_STATISTICS)/$(OBJ3).c
	ar r  $@  $(OBJ3).o
	@echo "*** Making $@ ***"
	@echo ""

clean:
	rm -f *.o $(TARGET) *.hi
	rm -f $(PATH_STATISTICS)/libstatisticsUtils.a
.cc.o:
	gcc -o $<

