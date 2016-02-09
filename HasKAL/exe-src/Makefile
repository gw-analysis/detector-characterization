
# compiler option
HC = ghc -O2

# use library
USELIB= libframe

# program
TAR1= plottimeseries
TAR2= plotspectrum
TAR3= gps2localtime
TAR4= localtime2gps
TAR5= showChannels
TAR6= showChannelandSamplingRate
TAR7= showKagraData
TAR8= kagraDailyDataFind
TAR9= updateFrameFullDB
TAR10= rayleighMon
TAR11= studentRayleighMon
TARs= ${TAR1} ${TAR2} ${TAR3} ${TAR4} ${TAR5} ${TAR6} ${TAR7} ${TAR8} ${TAR9} ${TAR10} ${TAR11}

# dependency
DEP1= ./HasKAL/PlotUtils/HROOT/AppendFunction.cc
DEPs= ${DEP1}

#########################################
# link path
ifneq (${USELIB},)
CFLAGS=`pkg-config --cflags ${USELIB}`
LDFLAGS=`pkg-config --libs-only-L ${USELIB}`
LIBS=`pkg-config --libs-only-l ${USELIB}`
endif

# compile rule
all: ${TARs}

${TAR1}: ${TAR1}.hs ${DEPs}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

${TAR2}: ${TAR2}.hs ${DEPs}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

${TAR3}: ${TAR3}.hs
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

${TAR4}: ${TAR4}.hs
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

${TAR5}: ${TAR5}.hs
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

${TAR6}: ${TAR6}.hs
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

${TAR7}: ${TAR7}.hs ./HasKAL/LineUtils/LineRemoval/rng_median.c
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} -fPIC

${TAR8}: ${TAR8}.hs ./HasKAL/LineUtils/LineRemoval/rng_median.c
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} -fPIC


${TAR9}: ${TAR9}.hs ./HasKAL/LineUtils/LineRemoval/rng_median.c
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS} -fPIC

${TAR10}: ${TAR10}.hs ${DEPs}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

${TAR11}: ${TAR11}.hs ${DEPs}
	${HC} -o $@ $^ ${CFLAGS} ${LDFLAGS} ${LIBS}

clean:
	rm -f ./*~ ./*.o ./*.hi ./*.dyn_o ./*.dyn_hi

cleanall: clean
	rm -f ${TARs}
	rm -f ./HasKAL
