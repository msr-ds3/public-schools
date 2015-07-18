#!/bin/bash

if [ $# -lt 1 ]
then
    echo "usage: $0 <borough>"
    exit 1
fi

borough=$1

if [ $borough == "manhattan" ]
then
    area=1
elif [ $borough == "brooklyn" ]
then
    area=2
fi
output_dir=streeteasy/$borough
[ -d $output_dir ] || mkdir -p $output_dir
cd $output_dir


base_url="http://streeteasy.com/nyc/process/sales/xls/area:${area}|school:"
awk -F, -v u=$base_url 'NR > 1 {print u$2}' ../../schools/elementary_schools_${borough}.csv | \
    head -n 3 | \
    xargs wget --wait=30 --random-wait
