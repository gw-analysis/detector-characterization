#******************************************#
#     File Name: Makefile
#        Author: Takahiro Yamamoto
# Last Modified: 2015/08/25 20:46:01
#******************************************#

# compiler option
HC = ghc --make -O2

# program
TAR1=sample1.cgi
TAR2=sample2.cgi
TARs= ${TAR1} ${TAR2}

# compile rule
all: ${TARs}

${TAR1}: ${TAR1}.hs
	${HC} -o $@ $^

${TAR2}: ${TAR2}.hs
	${HC} -o $@ $^

clean:
	rm -f ./*~ ./*.o ./*.hi

cleanall: clean
	rm -f ${TARs}

