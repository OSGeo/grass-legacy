#!/bin/sh

i=0
while [ $i -le 20 ]
do
	mknod fifo.${i}a p
	mknod fifo.${i}b p

	i=`expr $i + 1`
done
chmod 0666 fi*
