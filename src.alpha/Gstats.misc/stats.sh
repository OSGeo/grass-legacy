:
while test $# != 0
do
	case "$1" in
	-z) z=z;shift;;
	-v) v=v;shift;;
	-zv|-vz) z=z;v=v;shift;;
	-|-*) oops=yes;break;;
	*) break;;
	esac
done
if test $# != 1 -o "$oops" = yes
then
	echo "Usage: `basename $0` [-vz] cellfile" >&2
	exit 1
fi
eval `Gfindfile cell "$1"`
if [ ! "$file" ]
then
    echo `basename $0`: "$1" - not found >&2
    exit 1
fi
Gstats -c$z$v "$1" | awk -F: '
   BEGIN{sum=0.0;sum2=0.0}
   NR==1{min=$1; max=$1}
	{sum += $1 * $2; sum2 += $1 * $1 * $2; N += $2}
	{if($1 > max) max = $1; if ($1 < min) min = $1}
	{if($2 > modecount) {mode=$1; modecount=$2}}
     END{
         print "min      ", min
	 print "max      ", max
	 print "mean     ", sum/N
	 print "mode     ", mode
	 print "deviation", sqrt((sum2 - sum*sum/N)/N)
        }'
exit $?
