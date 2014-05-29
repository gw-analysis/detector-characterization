#******************************************#
#     File Name: guiTest.mk
#        Author: Takahiro Yamamoto
# Last Modified: 2014/05/29 11:23:29
#******************************************#

# compiler/option
HC = ghc -O2
LIBS = -lstdc++ -lFrame

# program
TAR1 = guiTest
TARs = ${TAR1}

# dependency
PREF = ./HasKAL/GUI_Utils/GUI
DEPs = ${PREF}_Utils.hs \
       ${PREF}_Supplement.hs \
       ${PREF}_RangeRingDown.hs \
       ${PREF}_RangeInspiral.hs \
       ${PREF}_RangeIMBH.hs \
       ${PREF}_GlitchKleineWelle.hs \
       ${PREF}_GaussianityRayleighMon.hs

# temp file
TEMP = ./*~ ${PREF}*~ \
       ./*.o ${PREF}*.o \
       ./*.hi ${PREF}*.hi

# compile rule
all: ${TARs}

${TAR1}: ${TAR1}.hs ${DEPs}
	${HC} -o $@ $< ${LIBS}

clean:
	rm -f ${TEMP}

cleanall: clean
	rm -f ./optKW_* ${TARs} ./*.lst
	rm -fR ./KW_*

