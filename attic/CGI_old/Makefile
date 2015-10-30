HC = ghc --make

TAR1=pastDataViewer.cgi
TAR2=webMonitor.cgi
TAR3=multiChannelViewer.cgi
TAR4=multiChannelViewer2.cgi
TAR5=brucoDetail.cgi
TARs=${TAR1} ${TAR2} ${TAR3} ${TAR4} ${TAR5}

DEP=./HasKAL/PlotUtils/HROOT/AppendFunction.cc

RM_CMD:=/bin/rm -f
CHMOD_CMD:=/bin/chmod

all : ${TARs}

${TAR1} : ${TAR1}.hs ${DEP}
	${HC} -o $@ $^ -lFrame
	${CHMOD_CMD} 755 $@

${TAR2} : ${TAR2}.hs ${DEP}
	${HC} -o $@ $^ -lFrame
	${CHMOD_CMD} 755 $@

${TAR3} : ${TAR3}.hs ${DEP}
	${HC} -o $@ $^ -lFrame
	${CHMOD_CMD} 755 $@

${TAR4} : ${TAR4}.hs
	${HC} -o $@ $^ -lFrame
	${CHMOD_CMD} 755 $@

${TAR5} : ${TAR5}.hs ${DEP}
	${HC} -o $@ $^ -lFrame
	${CHMOD_CMD} 755 $@

clean :
	${RM_CMD} ./*.hi ./*.o ./*~ ./*.dyn_hi ./*.dyn_o

cleanall : clean
	${RM_CMD} ${TARs}

