:
#This shell was written by Michael Shapiro, USACERL


# if user already has a monitor selected, try to use it
monitor=`g.gisenv MONITOR`
g.gisenv MONITOR=
case "$monitor" in
    x*|X*) d.mon $monitor 2>/dev/null;;
esac
mon=`g.gisenv MONITOR`
if test .$mon != .
then
    echo $mon
    exit 0
fi

# now we go thru the monitorcap looking for available x-monitors

d.mon -l | awk 'NR < 3 {next} $1 ~ /^[xX]/ {print $1}'| (
    while read monitor
    do
	d.mon $monitor 2>/dev/null
	mon=`g.gisenv MONITOR`
	if test .$mon != .
	then
	    break
	fi
    done
)
mon=`g.gisenv MONITOR`
if test .$mon != .
then
    echo $mon
    exit 0
fi

echo "Can't start any monitor for you" 1>&2
exit 1
