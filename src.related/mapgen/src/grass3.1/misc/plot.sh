#!/bin/sh
CHECK.MAP
if test $? = 1
	then
	exit
fi
tput clear
list=`awk '{printf "%s ", $1}' $GRAPH/etc/PLOTTER`
echo -n "Enter the plotter name ($list): "
read printer
echo "	Enter the paper size:

	a= 8.5 x 11
	b= 11 x 17
	c= 17 x 22
	d= 22 x 34
	e= 34 x 44
	f= non standard paper"
echo
while (test "$size" != "a" -a "$size" != "b" -a "$size" != "c" -a "$size" != "d" -a "$size" != "e" -a "$size" != "f")
do
	echo -n "SIZE: "
read size
done
plotter=`grep $printer $GRAPH/etc/PLOTTER | awk '{print $2}'`
if test "$size" = "f"; then
	ttydev=`grep $printer $GRAPH/etc/PLOTTER | awk '{print $3}'`
	plotter -d $plotter $* > $ttydev < $ttydev
else
	plotter=$plotter$size
	plotter -d $plotter $* | lp -d$printer
fi
