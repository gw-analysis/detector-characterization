

#!/bin/sh


#YESTERDAY=`date -d '1 day ago' "+%Y %m %d"`
YESTERDAY="2015-12-19"
MIRROR_SERVER="detchar@seikai.hep.osaka-cu.ac.jp"
filelist=`kagraDailyDataFind ${YESTERDAY} JST`
if test filelist = "Nothing"; then
else
  for var in ${filelist}
  do
    echo scp ${var} ${MIRROR_SERVER}:/data/kagra/raw/full/
    echo ssh ${MIRROR_SERVER} \"updateFrameFull /data/kagra/raw/full/`basename ${var}`\"
  done
fi


