#******************************************#
#     File Name: guiTest.mk
#        Author: Takahiro Yamamoto
# Last Modified: 2014/04/28 10:16:33
#******************************************#

# compiler option
HC = ghc -O

# program
TAR1 = guiTest
TARs = ${TAR1}

# temp file
TILs = ./*~ ./HasKAL/GUI_Utils/*~
OBJs = ./*.o ./HasKAL/GUI_Utils/*.o
INFs = ./*.hi ./HasKAL/GUI_Utils/*.hi

# compile rule
all: ${TAR1}

${TAR1}: ${TAR1}.hs ./HasKAL/GUI_Utils/GUI_Utils.hs
	${HC} -o $@ $< -lstdc++ -lFrame

clean:
	rm -f ${TILs}

cleanobs:
	rm -f ${TILs} ${OBJs} ${INFs}

cleanall:
	rm -f ./optKW_* ${TARs} ${TILs} ${OBJs} ${INFs}
	rm -fR ./KW_*

