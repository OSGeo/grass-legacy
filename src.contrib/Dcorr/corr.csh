#!/bin/csh -f

# prints a graph of the correlation between data layers (in pairs)
# Look for source under <grass>/src.local/Dcorr

onintr END
if ( $#argv < 2) then
	echo Usage: `basename $0` layer1 layer2 '[layer3 [layer4]]'
	exit 1
endif
if ( $#argv > 4 ) then
	echo max 4 layers allowed
	exit 1
endif

set ok=yes
foreach f ($*)
	eval `Gfindfile cell $f | sed 's/^/set /'`
	if ( "$name" == "" ) then
		echo $f not found
		set ok=no
	endif
end
if ( $ok == "no" ) exit 1

Derase
if ( $status != 0 ) exit 1
echo "CORRELATION" | Dtext white 4 1
set colors = (red green blue white gray purple)
set c = 1
set l = 2
set i = 1
while ( $i < $#argv )
   @ j = $i + 1
   while ( $j <= $#argv )
     echo $argv[$i] $argv[$j] | Dtext $colors[$c] 4 $l
	 Gstats -c $argv[$i] $argv[$j]  > /tmp/corr.$$
	 set m = (`awk -F: '$1>max1{max1=$1} $2>max2{max2=$2} min1==0||$1<min1{min1=$1} min2==0||$2<min2{min2=$2} END {print min1,max1,min2,max2}' /tmp/corr.$$`)

	 awk -F: '{print "move",($1-min1+1)*100.0/(max1-min1+1),($2-min2+1)*100.0/(max2-min2+1);print "draw",($1-min1+1)*100.0/(max1-min1+1),($2-min2+1)*100.0/(max2-min2+1) }' min1=$m[1] max1=$m[2] min2=$m[3] max2=$m[4] /tmp/corr.$$ | Dgraph color=$colors[$c]
     @ j++
     @ c++
     @ l++
   end
   @ i++
end
    rm -f /tmp/corr.$$
    exit 0

END:
    rm -f /tmp/corr.$$
    exit 1
