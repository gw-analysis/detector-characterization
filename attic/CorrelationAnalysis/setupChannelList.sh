#!/bin/sh
# usage setupChannelList.sh  filename  samplingrate

#/usr/bin/FrChannels $1 | grep $2 | cut -d" " -f1 > channelList.txt
/usr/bin/FrChannels $1 | grep $2 > channelList.txt
