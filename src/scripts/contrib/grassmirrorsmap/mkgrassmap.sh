#!/bin/bash

##############################################################################
#
#  mkgrassmap.sh   
#  Tom Poindexter <tpoindex@nyx.net>  Copyright 2001
#  released under the GNU Public License (GPL)
#  
#  mkgrassmap.sh draws a world map and hilights countries that provide a
#  primary GRASS web site or mirror.  a corresponding html image map is
#  generated for the hilighted areas.  see below for input file names and
#  specifications.
#  
#  mkgrassmap.sh depends on the 'wdbtemp4' GRASS vector file created from
#  UNEP sources.  see the releated 'list_countries.sh' shell script to
#  list country names from that file.
#
#  grass side effects:
#  starts, selects, and stops HTMLMAP & CELL monitors
#  creates and deletes temporary vector files beginning with 'temp__*'
#  creates and deletes raster file D_cell 
#  resets region to default
#
#############################################################################

# check GISBASE for running under GRASS

if test "$GISBASE" = "" ; then
    echo "You must be running under GRASS to execute $0"
    exit 1
fi

# set output for grass commands, normally /dev/null, change to /dev/tty
# (or /dev/stdout or /dev/fd/1 if supported on your system) for debugging
out=/dev/null

# get current grass env
eval `g.gisenv`

: $GISDBASE $LOCATION

# grass web site files
# format:
#  country[|country[|country  ...]]=url
#  e.g.
#  GERMANY|FRANCE|SPAIN=http://www.geog/uni-hannover.de/grass/index2.html
#  UNITED STATES=http://www.baylor.edu/~grass/index2.html
#
#  (the country list is just an agrument to 'egrep', so you can
#   use regular expressions)

# grass web site files
grass_sites="grass.sites.main grass.sites.mirror"

# must be one map color for each map file (see coastline labels & colors below)
map_colors="green orange"

# world vector map
world_vec=wdbtemp4

GRASS_HEIGHT=320
GRASS_WIDTH=640
GRASS_HTMLFILE=grass_sites.htmlmap
export GRASS_HEIGHT GRASS_WIDTH GRASS_HTMLFILE

# name of .gif file to create for map
giffile=grass_sites.gif

# remove any old htmlmap file & .gif
/bin/rm -f $GRASS_HTMLFILE $giffile

# set region to default, assumed to cover entire world area
g.region  -dp res=0:00:36  >$out 2>&1

# start up cell and htmlmap drivers, sleep needed to allow driver startup!
echo "starting HTMLMAP and CELL drivers...."
g.remove rast=D_cell  >$out 2>&1
d.mon start=HTMLMAP   >$out 2>&1
sleep 5
d.mon start=CELL      >$out 2>&1
sleep 5

# draw background, blue oceans
d.mon select=CELL    >$out 2>&1
sleep 2
d.erase color=blue   >$out 2>&1

# extract coastline vectors, all but the UNKNOWN category
echo "extracting and drawing coastline areas..."

coast=`grep -v "UNKNOWN"  $LOCATION/dig_cats/$world_vec | tail +5 | \
    cut -f1 -d: | tr "\012" "," | sed -e 's/,$//'`
g.remove vect=temp__coast   >$out 2>&1
v.extract -d type=area input=$world_vec output=temp__coast list="$coast" \
    new=2     >$out 2>&1
d.area map=temp__coast linecolor=gray fillcolor=gray  >$out 2>&1
g.remove vect=temp__coast    >$out 2>&1

# label the image, be sure to change colors & names to reflect site categories
echo "labeling map..."
d.font font=romans  >$out 2>&1
echo " GRASS Main Sites" | d.text size=5 color=green  line=17
echo " Mirror Sites"     | d.text size=5 color=orange line=18

# now extract and draw the site areas
color=1
g.remove vect=temp__vec >$out 2>&1

for site in $grass_sites
do
  echo ""
  echo "drawing areas for file: $site"
  areacolor=`echo "$map_colors" | awk "{print \\$$color ;}"`
  cat $site | while read line
  do
    IFSSAVE=$IFS
    IFS='='
    set -- $line
    IFS=$IFSSAVE
    countries="$1"
    url=`echo $2 | tr -d "\012"`

    echo ""
    echo "  url: $url"
    echo "  extracting and drawing: $countries"
    vec_list=`egrep -i "$countries" $LOCATION/dig_cats/$world_vec | \
	cut -f1 -d: | tr "\012" "," | sed -e 's/,$//'`

    v.extract -d type=area input=$world_vec output=temp__vec list="$vec_list" \
	new=2   >$out 2>&1

    # draw the image areas
    d.mon select=CELL  >$out 2>&1
    sleep 2
    d.area map=temp__vec linecolor=$areacolor fillcolor=$areacolor  >$out 2>&1

    # draw the htmlmap areas
    d.mon select=HTMLMAP     >$out 2>&1
    sleep 2
    echo $url | tr -d "\012" | d.text  >$out 2>&1
    d.area map=temp__vec     >$out 2>&1
    g.remove vect=temp__vec  >$out 2>&1

  done
  color=`expr $color + 1`

done

# save the CELL and htmlmap
echo ""
echo "stopping drivers..."
d.mon stop=CELL         >$out 2>&1
sleep 5
d.mon stop=HTMLMAP      >$out 2>&1
sleep 5

# convert the CELL into a graphic format

echo "converting map to gif..."

g.region raster=D_cell  >$out 2>&1
r.out.ppm -q input=D_cell output=$giffile.ppm  >$out 2>&1
ppmtogif $giffile.ppm >$giffile 2>$out
/bin/rm -f $giffile.ppm
g.remove rast=D_cell  >$out 2>&1

# restore region 
g.region -d  >$out 2>&1

# now generate some simple test html

echo "<html><head><title>GRASS Web Sites</title></head>"         >grassmap.html
echo "<body><h1>GRASS Web Sites</h1><br><br>"                   >>grassmap.html
echo "<img src=\"$giffile\" usemap=\"#map\" alt=\"grass\">"     >>grassmap.html
cat $GRASS_HTMLFILE                                             >>grassmap.html
echo "<br><br><br></body></html>"                               >>grassmap.html


echo "done."
echo "gif file is:     $giffile"
echo "htmlmap file is: $GRASS_HTMLFILE"
echo "sample html file: grassmap.html"
exit
