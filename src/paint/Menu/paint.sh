:
: ${GISBASE?}

ETC=$GISBASE/etc
PAINT=$ETC/paint

# paint: interface to the interactive paint routines

painter=`p.select -p`
if [ ! "$painter" ]
then
    p.select
    painter=`p.select -p`
    if [ ! "$painter" ]
    then
	exit
    fi
fi

choice=${1-none}

while test 1
do

case $choice in

    select)     p.select;	
		painter=`p.select -p`
		if [ ! "$painter" ]
		then
		    exit
		fi
		;;
    chart)	exec p.chart ;;
    colors)	exec p.colors ;;
    icons)	exec p.icons ;;
    labels)	exec p.labels ;;
    map)	exec p.map ;;
    ctest)	exec $PAINT/char.test ;;
    ptest)	exec $PAINT/pixel.test ;;
    htest)	exec $PAINT/hres.test ;;
    vtest)	exec $PAINT/vres.test ;;
    exit | "") exit;;

esac

echo 'Please select one of the following options'
echo ''
echo '   map      (to paint a map)'
echo '   chart    (to paint the printer color chart)'
echo '   colors   (to modify the color table for a raster file)'
echo '   icons    (to create or modify an icon file for paint map)'
echo '   labels   (to create or modify a labels file for paint map)'
echo '   select   (to select a paint output device)'
echo ''
echo '   exit     (to exit from paint)'
echo ''

$ETC/echo -n 'selection: '
read choice
done
