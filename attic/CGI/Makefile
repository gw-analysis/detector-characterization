HC = ghc --make

TAR1=pastDataViewer.cgi
TAR2=webMonitor.cgi

DEP=./HasKAL/PlotUtils/HROOT/AppendFunction.cc

RM_CMD:=/bin/rm -f
CHMOD_CMD:=/bin/chmod

all : ${TAR1} ${TAR2}

${TAR1} : ${TAR1}.hs
	${HC} -o $@ $^
	${CHMOD_CMD} 755 $@

${TAR2} : ${TAR2}.hs ${DEP}
	${HC} -o $@ $^ -lFrame
	${CHMOD_CMD} 755 $@

clean :
	${RM_CMD} ./*.hi ./*.o ./*~

cleanall : clean
	${RM_CMD} ${TAR1} ${TAR2}

