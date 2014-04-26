#******************************************#
#     File Name: rayleighMonTest.mk
#        Author: Takahiro Yamamoto
# Last Modified: 2014/04/10 21:04:49
#******************************************#

# compiler option
HC = ghc -O

# program
TAR1 = rayleighMonTest
TARs = ${TAR1}

# temp file
TILs = ./*~ 
OBJs = ./*.o 
INFs = ./*.hi 

# compile rule
all: ${TAR1}

${TAR1}: ${TAR1}.hs
	${HC} -o $@ $< -lstdc++ -lFrame

clean:
	rm -f ${TILs}

cleanobs:
	rm -f ${TILs} ${OBJs} ${INFs}

cleanall:
	rm -f ${TARs} ${TILs} ${OBJs} ${INFs} ${TAR1}Result.txt
