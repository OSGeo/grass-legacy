

#!/bin/sh
cd ${GISBASE?}/etc/demo.scripts
: ${LOCATION_NAME?} ${LOCATION?}
trap "" 2 3
MAXVAL=5

if [ $LOCATION_NAME != spearfish ]
then
	echo
	echo "Sorry, to run this program you must be working inside"
	echo "the <spearfish> sample data base."
	echo
	exit
fi
# clear the screen and make sure that we can get to the graphics monitor
Dclear.screen || exit 1

Dcolormode fixed


# Save the current window
cp $LOCATION/WIND $$.wind
Gwindow layer=geology
Gwindow res=100

until
Dclear.screen
Dnew a 50 99 50 90
Dchoose a
Dgrass.logo
Dfont romans
i=`Dmenu << 'EOF'
# set the text size in % of entire screen height
.S 3
# set the top edge
.T 10
# set the LEFT edge
.L 10
Choose an option with MOUSE
  Executive Overview
  What is a GIS
  GRASS Data
  GRASS Capabilities
  Quit
EOF
`
if [ $i -ge $MAXVAL ] 
	then
	cp $$.wind $LOCATION/WIND
	rm -f $$.wind
	exit 1
fi

$i

do
echo "" > /dev/null
done
