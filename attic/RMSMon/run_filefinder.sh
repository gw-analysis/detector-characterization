#!/bin/sh

FNAME=filefinder_log.txt

date >>$FNAME
DAY=16
./FileFinder 2015 12 ${DAY}
echo "./FileFinder 2015 12 ${DAY}" >> ${FNAME}
date >>${FNAME}
date >>$FNAME
DAY=17
./FileFinder 2015 12 ${DAY}
echo "./FileFinder 2015 12 ${DAY}" >> ${FNAME}
date >>${FNAME}
date >>$FNAME
DAY=18
./FileFinder 2015 12 ${DAY}
echo "./FileFinder 2015 12 ${DAY}" >> ${FNAME}
date >>${FNAME}
date >>$FNAME
DAY=19
./FileFinder 2015 12 ${DAY}
echo "./FileFinder 2015 12 ${DAY}" >> ${FNAME}
date >>${FNAME}
date >>$FNAME
DAY=20
./FileFinder 2015 12 ${DAY}
echo "./FileFinder 2015 12 ${DAY}" >> ${FNAME}
date >>${FNAME}
date >>$FNAME
DAY=21
./FileFinder 2015 12 ${DAY}
echo "./FileFinder 2015 12 ${DAY}" >> ${FNAME}
date >>${FNAME}
