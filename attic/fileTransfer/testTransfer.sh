

for var in `kagraDailyRawFileList ${YESTERDAY}`
do
  scp ${var} ${MIRROR_SERVER}:/data/kagra/raw/full/
　filename=`basename ${var}`
  ssh ${MIRROR_SERVER} "updateFrameFull /data/lagra/raw/full/$var"
done



