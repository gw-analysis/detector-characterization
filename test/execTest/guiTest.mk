#******************************************#
#     File Name: guiTest.mk
#        Author: Takahiro Yamamoto
# Last Modified: 2014/04/18 16:22:18
#******************************************#

# compiler option
HC = ghc -O

# program
TAR1 = guiTest
TARs = ${TAR1}

# temp file
TILs = ./*~ ./HasKAL/GUI_Utils/*~ ./HasKAL/MonitorUtils/*~ ./HasKAL/ExternalUtils/*~
OBJs = ./*.o ./HasKAL/GUI_Utils/*.o ./HasKAL/MonitorUtils/*.o ./HasKAL/ExternalUtils/*.o
INFs = ./*.hi ./HasKAL/GUI_Utils/*.hi ./HasKAL/MonitorUtils/*.hi ./HasKAL/ExternalUtils/*.hi

# compile rule
all: ${TAR1}

${TAR1}: ${TAR1}.hs ./HasKAL/GUI_Utils/GUI_Utils.hs ./HasKAL/MonitorUtils/EXTKleineWelle.hs ./HasKAL/ExternalUtils/Lwtprint.hs
	${HC} -o $@ $< -lstdc++ -lFrame

clean:
	rm -f ${TILs}

cleanobs:
	rm -f ${TILs} ${OBJs} ${INFs}

cleanall:
	rm -f ./optKW_* ${TARs} ${TILs} ${OBJs} ${INFs}
	rm -fR ./KW_*

