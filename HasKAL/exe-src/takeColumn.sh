#!/bin/sh
ncol=$1
#echo $ncol
awk -v ncol=$ncol '{print $ncol}'
