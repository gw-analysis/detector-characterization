

#!/bin/sh


#YESTERDAY=`date -d '1 day ago' "+%Y %m %d"`
#YESTERDAY="2016-12-19"
YESTERDAY=$1
MIRROR_SERVER="detchar@seikai.hep.osaka-cu.ac.jp"
filelist=`kagraDailyDataFind ${YESTERDAY} JST`
DBCMD="${HOME}/tools/detector-characterization/HasKAL/exe-src/updateFrameFullDB"
if test ${filelist} = \"Nothing\" ; then
  echo "file not found."
  exit 1
else
  for var in ${filelist}
  do
    scp ${var} ${MIRROR_SERVER}:/data/kagra/raw/full/
    ssh ${MIRROR_SERVER} "${DBCMD}" "/data/kagra/raw/full/`basename ${var}`"
  done
fi
exit 0

