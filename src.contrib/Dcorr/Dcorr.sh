
#  Modified version from SCS  Ron Glenn

# prints a graph of the correlation between data layers (in pairs)

if test $# -lt 2 
   then echo Usage: `basename $0` layer1 layer2 '[layer3 [layer4]]'
	exit 1
fi

if test $# -gt 4 
   then echo "maximum of 4 layers allowed"
	exit 
fi

ok=yes
for f in $*
do

	eval `Gfindfile cell $f`
	if [ "$name" = "" ] 
	   then echo "$f not found"
		ok=no
	fi
done

if [ "$ok" = "no" ]
   then exit
fi

status=`Derase`

if [ "$status" = 0 ]
   then exit
fi

echo "CORRELATION" | Dtext white 4 1
c=1
l=2
i=1
while [ "$i" -lt $# ]
do
   j=`expr "$i" + 1`
   while [ "$j" -le $# ]
   do
     if [ "$i" -eq 1 ]
       then argv1=$1
     elif [ "$i" -eq 2 ]
       then argv1=$2
     elif [ "$i" -eq 3 ]
       then argv1=$3
     elif [ "$i" -eq 4 ]
       then argv1=$4
     fi
     if [ "$j" -eq 1 ]
       then argv2=$1
     elif [ "$j" -eq 2 ]
       then argv2=$2
     elif [ "$j" -eq 3 ]
       then argv2=$3
     elif [ "$j" -eq 4 ]
       then argv2=$4
     fi
     if [ "$c" -eq 1 ]
       then colors=red
     elif [ "$c" -eq 2 ]
       then colors=green
     elif [ "$c" -eq 3 ]
       then colors=blue
     elif [ "$c" -eq 4 ]
       then colors=white
     elif [ "$c" -eq 5 ]
       then colors=gray
     elif [ "$c" -eq 6 ]
       then colors=purple
     fi
     
     echo "$argv1 $argv2" | Dtext $colors 4 $l
     Gstats -c $argv1 $argv2  > /tmp/corr.$$
     m=`awk -F: '$1>max1{max1=$1} $2>max2{max2=$2} min1==0||$1<min1{min1=$1} min2==0||$2<min2{min2=$2} END {print min1,max1,min2,max2}' /tmp/corr.$$`
     
     kk=0
     for k in $m
     do
     if [ "$kk" -eq 0 ]
        then m1=$k
     elif [ "$kk" -eq 1 ]
        then m2=$k
     elif [ "$kk" -eq 2 ]
        then m3=$k
     elif [ "$kk" -eq 3 ]
        then m4=$k
     fi
     kk=`expr "$kk" + 1`
     done

     awk -F: '{printf("move %4.2f %4.2f\n",($1-min1+1)*100.0/(max1-min1+1),($2-min2+1)*100.0/(max2-min2+1));printf("draw %4.2f %4.2f\n",($1-min1+1)*100.0/(max1-min1+1),($2-min2+1)*100.0/(max2-min2+1)) }' min1=$m1 max1=$m2 min2=$m3 max2=$m4 /tmp/corr.$$ | Dgraph color=$colors
     j=`expr "$j" + 1`
     c=`expr "$c" + 1`
     l=`expr "$l" + 1`
   done
   i=`expr "$i" + 1`
done

rm -f /tmp/corr.$$
exit 

