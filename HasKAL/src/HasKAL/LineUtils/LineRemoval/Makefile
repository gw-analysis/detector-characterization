CC = ghc
PROGS = main_Mblt
LOADLIBES = ./rng_median.c HasKAL/SignalProcessingUtils/filterFunctions.c

OBJS1 = main_Mblt.hs

all:$(PROGS)

main_Mblt:	$(OBJS1)
	$(CC) --make $(OBJS1) $(LOADLIBES)

