#!/bin/sh

me=`basename $0`
if test $# -lt 2 -o $# -gt 3
then
    echo "Usage: $me basemap [@]valuemap [resultmap]" >&2
    echo "       @ means use values from cats file for valuemap" >&2
    echo "         (otherwise use category values directly)" >&2
    exit 1
fi

basemap="$1"
eval `Gfindfile cell "$basemap"`
if test "$mapset" = ""
then
    echo "$me: $basemap - cell file not found" >&2
    exit 1
fi

case "$2" in

    @*) valuemap="`echo $2 | sed 's/@ *//'`"
            usecats=yes
        ;;
     *) valuemap="$2"
        usecats=no
        ;;
esac

eval `Gfindfile cell "$valuemap"`
if test "$mapset" = ""
then
    echo "$me: $valuemap - cell file not found" >&2
    exit 1
fi

resultmap=$3

if test "$resultmap" != ""
then
    echo "basemap=$basemap valuemap=$valuemap resultmap=$resultmap" >&2
else
    echo "basemap=$basemap valuemap=$valuemap" >&2
fi

stats=/tmp/stats.$$
cats=/tmp/cats.$$
temp1=/tmp/temp1.$$
temp2=/tmp/temp2.$$

trap "rm -f $stats $cats $temp1 $temp2;exit 1" 2 3 15

Gstats "$basemap" "$valuemap" |\
   awk -F: '$1!=0&&$2!=0{print}' |\
   sort -t: -n +0 -1 \
 > $stats

if test $usecats = yes
then
    Gcats "$valuemap" `awk -F: '{print $2}' $stats | sort -u` |\
      awk '{v=$2+0.0;print $1,v}' > $cats

    awk '{print "$2==" $1 "{print $1 \":" $2 ":\" $3}"}' $cats > $temp1
    awk -F: -f $temp1 $stats > $temp2
    mv $temp2 $stats
fi

# create the average for each category and output reclass rules


awk -F: '$1 != prev {
        if (prev) { print prev,"=",prev,sum/count }
        count = 0.0 ; sum = 0.0
        prev = $1
    }
    {count+=$3 ; sum += $2*$3}
    END { print prev,"=",prev,sum/count }' $stats > $temp1

if test "$resultmap" != ""
then
    echo "Creating $resultmap" >&2
    Greclass "$basemap" "$resultmap" < $temp1
else
    cat $temp1
fi

rm -f $stats $cats $temp1 $temp2
