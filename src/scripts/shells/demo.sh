:
eval `g.gisenv`
cd ${GISBASE?}/scripts/demo.scripts
: ${LOCATION_NAME?}
trap "" 2 3
MAXVAL=5

if [ "$LOCATION_NAME" != spearfish ]
then
	echo
	echo "Sorry, to run this program you must be working inside"
	echo "the <spearfish> sample data base."
	echo
	exit
fi
# clear the screen and make sure that we can get to the graphics monitor
d.erase || exit 1

d.colormode fixed


# Save the current window
region="`g.region -gp`"
g.region raster=geology
g.region res=100

until
d.frame -e
d.erase
d.frame -c f=abc at=50,99,50,90
d.frame -s f=abc
grass.logo.sh
d.font romans
i=`d.menu << 'EOF'
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
	g.region $region
        d.frame -e
	exit 1
fi

./$i

do
echo "" > /dev/null
done
d.frame -e
