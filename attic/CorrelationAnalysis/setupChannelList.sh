#!/bin/sh
# usage setupChannelList.sh  filename  samplingrate

if [ $# -eq 2 ]; then

    #/usr/bin/FrChannels $1 | grep $2 | cut -d" " -f1 > channelList.txt
    FrChannels $1 | grep $2 > channelList.txt

else
    
    FrChannels $1 > channelList.txt
fi
