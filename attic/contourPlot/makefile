#makefile
CC=ghc
OBJ1=plot_contour.hs

TARGET=plot_contour

all:$(TARGET)

plot_contour:$(OBJ1)
	$(CC) --make -o $@ $(OBJ1)
	@echo "*** Making $@ ***"
clean:
	rm -f *.o *.hi
	rm -f  $(TARGET)
