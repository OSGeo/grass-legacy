
#!/bin/sh
tput clear
CHECK.MAP
if test $? = 1
	then
	exit
fi
Gask old "" dig "" tmp
. tmp
rm tmp
#echo -n "Enter the name of the ouput reference [$name]: "
#read out
#if test "$out" = ""
#	then
#	out=$name
#fi
out=$name

echo -n "Enter the type of projection (ie. aea): "
read proj
echo "Enter the projecton parameters (ie +lon_0=-96)"
echo -n :
read zone
	dig_to_geo $mapset $name |\
	proj +proj=$proj +inv $zone -  > ../dig_geo/$out
