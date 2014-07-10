#!/bin/sh
set -e
#******************************************#
#     File Name: dependencySearch.sh
#        Author: Takahiro Yamamoto
# Last Modified: 2014/07/10 19:16:50
#******************************************#

if test ! ${HASKALOPT}
then
    echo "Undefind \"\${HASKALOPT}\""
    exit 1
fi

if test $1
then
    module=$1
    echo "##### ${module}"
    for result in `grep -r "^import\ .*\.${module%.hs}\$\|^import\ .*\.${module%.hs}\ as" ${HASKALOPT}/../HasKAL/src/HasKAL/ | awk -F":" '{print $1}'`
    do
	echo "  ${result#*/HasKAL/src/}"
    done
    echo ""
else
    for filename in `find ${HASKALOPT}/../HasKAL/src/HasKAL/ -name "*.hs"`
    do
	module=${filename##*/}
	echo "##### ${module}"
	for result in `grep -r "^import\ .*\.${module%.hs}\$\|^import\ .*\.${module%.hs}\ as" ${HASKALOPT}/../HasKAL/src/HasKAL/ | awk -F":" '{print $1}'`
	do
	    echo "  ${result#*/HasKAL/src/}"
	done
	echo ""
    done
fi

	


