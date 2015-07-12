#******************************************#
#     File Name: Makefile
#        Author: Takahiro Yamamoto
# Last Modified: 2015/07/12 22:06:30
#******************************************#

# compiler option
HC = ghc -O2

# program
TAR1= main
TARs= ${TAR1}

DEPs=./HasKAL/PlotUtils/HROOT/AppendFunction.cc

# compile rule
all: ${TARs}

${TAR1}: ${TAR1}.hs ${DEPs}
	${HC} -o $@ $^ -lFrame

clean:
	rm -f ./*~ ./*.o ./*.hi

cleanall: clean
	rm -f ${TARs}

