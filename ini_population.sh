#!/bin/sh

i=1
s=$(shuf -i0-4096 -n1)
while [ $i -le $1 ]
do
	xterm -e "./agent $i $s" &
	i=`expr $i + 1`
done
