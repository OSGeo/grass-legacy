##########################################################################
#
# savedisp.tcl
#
# Saves displayed maps to files in  
# GIS Manager 2: GUI for GRASS 6 
#
# Author: Michael Barton (Arizona State University)
#
# January 2006
#
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################

source $gmpath/montool.tcl
source $gmpath/tree.tcl
source $gmpath/group.tcl
source $gmpath/cmonitor.tcl
source $env(GISBASE)/etc/gtcltk/gmsg.tcl
source $env(GISBASE)/etc/gtcltk/select.tcl
source $env(GISBASE)/etc/gui.tcl

set bgcolor HoneyDew2

namespace eval cmon {
	variable array can # mon
	variable array cmon # mon
	variable array cmwd # mon
	variable array cmht # mon
	variable array canvas_geom # mon dim
    variable cmstatus
	}

set initwd 640
set initht 480
set east 0
set north 0
image create photo mapimg.$mon

###############################################################################

# Create window and canvas for display
proc cmon::create { } {
    global gmpath
    global bgcolor
    global outtext
    global env
    global initwd
    global initht
    global east 
    global north
    global coords
    global mapframe
    global b1east b1north
    global tree_pane
    global mon
    global win

	variable cmon
	variable can
	variable cmwd
	variable cmht
	variable canvas_geom
#	variable tree
	
	# Initialize window and map geometry
	
	set cmwd($mon) $initwd
	set cmht($mon) $initht
	set env(GRASS_WIDTH) $initwd
	set env(GRASS_HEIGHT) $initht
	
	set win ""


	# Create canvas monitor as top level mainframe
	toplevel .dispsave

    set savedisp [MainFrame .dispsave.savedisp \
   		-background $bgcolor -textvariable cmon::cmstatus ]


    # toolbar creation
    set cmon_tb  [$mapframe addtoolbar]
    CmonToolBar::create $cmon_tb

    
	# canvas creation
    set can($mon) [canvas $mapframe.can \
        -background #ffffff -borderwidth 0 -closeenough 1.0 \
        -insertbackground black -relief ridge -selectbackground #c4c4c4 \
        -selectforeground black -width $cmwd($mon) -height $cmht($mon) ]
    
    ###################
    # SETTING GEOMETRY
    ###################
    place $mapframe.can \
        -in $mapframe -x 0 -y 0 -anchor nw \
        -bordermode ignore 
	
#    set cmon::cmstatus [G_msg "TclTk Canvas Display Monitor $mon"]
#    $mapframe showstatusbar cmstatus 

    pack $mapframe -expand yes -fill both -ipadx 0 -ipady 0
	pack $cmon_tb -expand no -fill x -anchor nw -side bottom
	pack $mapframe.can -fill both -expand yes -anchor n -side top	
 
    set fon [font create -family Verdana -size 12 ]
    DynamicHelp::configure -font $fon -background yellow

	cmon::coordconv $mon 

# bindings for display canvas

#	mouse handlers
	bind $can($mon) <ButtonPress-1> {
		global currmon mon b1east b1north win
		variable tree		
		set winx [winfo pointerx .]
		set winy [winfo pointery .]
		set win [winfo containing $winx $winy]
		regexp -nocase {.*\((\d*)(\).*)} $win win1 currmon win2
		set mon $currmon
		#GmTree::saveload $mon
		set b1east  [cmon::scrx2mape %x]
		set b1north [cmon::scry2mapn %y]
		GmTree::switchpage $mon
	}
	

	bind $can($mon) <Motion> {
		set scrxmov %x
		set scrymov %y
		set eastcoord [eval cmon::scrx2mape %x]
		set northcoord [eval cmon::scry2mapn %y]
		set coords "$eastcoord $northcoord"
	}


    # indicator creation	
    set cmon_ind  [$mapframe addindicator -textvariable coords \
    	-width 25 -justify left -padx 15]
    	
	#GmTree::create $mon

#    update idletasks    


#	window configuration change handler for resizing
    bind $can($mon) <Configure> {
		set canvas_geom($mon,w) %w
		set scrwidth %w
		set canvas_geom($mon,h) %h
		set order %d
		regexp -nocase {.*\((\d*)(\).*)} $win win1 changemon win2
		#set mon $currmon
		if { $order == "below" } { set leavemon $changemon }
		after cancel cmon::do_resize $mon
		after idle cmon::do_resize $mon
	}
	
}






############################################################################
#
# MODULE:	savefile.sh for GRASS 6 GIS Manager 2 (2006/1/14)
# AUTHOR(S):	Michael Barton 
#		 Routine to read current display geometry written by 
#		 Hamish Bowman & Glynn Clements
# PURPOSE:	To export currently displayed maps to graphics file output
# COPYRIGHT:	(C) 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
#############################################################################


#%Module
#%  description: Saves active display monitor to graphics file in home directory
#%End
#%option
#% key: output
#% type: string
#% description: Name of output file (do NOT add extension)
#% required : yes
#%end
#%option
#% key: res
#% type: integer
#% answer: 1
#% description: Resolution of output file (single=1, double=2, quad=4)
#% required : no
#%end
#%option
#% key: format
#% type: string
#% options: png,ppm,tif,jpg,bmp
#% answer: png
#% description: Graphics file format
#% required : yes
#%end
#%option
#% key: compression
#% type: string
#% options: 0,1,9
#% answer: 0
#% description: Compression for PNG files (0=none, 1=fastest, 9=most)
#% required : no
#%end
#%option
#% key: quality
#% type: string
#% answer: 75
#% description: Size/quality for JPEG files (10=smallest/worst, 100=largest/best)
#% required : no
#%end
#%flag 
#%key: t
#%description: Transparent background
#%end
#%flag 
#%key: b
#%description: Set background color to black (white default)
#%end


if  [ -z $GISBASE ] ; then
 echo "You must be in GRASS GIS to run this program."
 exit 1
fi   

if [ "$1" != "@ARGS_PARSED@" ] ; then
  exec g.parser "$0" "$@"
fi


# Save old settings
old_GRASSW=`eval g.gisenv get=GRASS_WIDTH`
old_GRASSH=`eval g.gisenv get=GRASS_HEIGHT`
old_TRANSPARENT=`eval g.gisenv get=GRASS_BACKGROUNDCOLOR`
old_BACKGROUND=`eval g.gisenv get=GRASS_TRANSPARENT`
old_COMPRESSION=`eval g.gisenv get=GRASS_PNG_COMPRESSION`


curr_mon=`d.mon -p | awk '{printf "%s", $4}'`

#setting variables (get rid of extra png)
#output="`basename $GIS_OPT_output .png`"
res="$GIS_OPT_res"

#get GRASS version for display monitor name
GRASS_VER=`g.version | awk '{printf "%s", $1 " " $2}'`

#capture current display monitor and geometry
# identify current monitor
currmon=`eval d.mon -L | grep "(selected)" | awk '{print $1}'`
curr_width=$old_GRASSW
curr_height=$old_GRASSH

#set resolution for PNG output
out_width=`expr $res \* $curr_width`
out_height=`expr $res \* $curr_height`

#set window geometry for PNG output
unset GRASS_WIDTH
unset GRASS_HEIGHT
export GRASS_WIDTH=$out_width
export GRASS_HEIGHT=$out_height

#make sure that output is 24 bit color
export GRASS_TRUECOLOR=TRUE

#set other output options

if [ "$GIS_FLAG_b" -eq 1 ] ; then
    export GRASS_BACKGROUNDCOLOR=000000
else
    export GRASS_BACKGROUNDCOLOR=FFFFFF
fi

if [ "$GIS_FLAG_t" -eq 1 ] ; then
    export GRASS_TRANSPARENT=TRUE
else
    export GRASS_TRANSPARENT=FALSE
fi

export GRASS_PNG_COMPRESSION=$GIS_OPT_compression

#set output file name and format
if [ "$GIS_OPT_format" = "png" ] ; then
    output=$GIS_OPT_output".png"
fi

if [ "$GIS_OPT_format" != "png" ] ; then
    output=$GIS_OPT_output".ppm"
fi

echo "******************"
echo "Saving display from [$GRASS_VER- Monitor: $curr_mon] to $output"
echo "in home directory at $out_width x $out_height resolution"
echo "******************"

export GRASS_PNGFILE=$output

#export display to PNG driver

if [ -n "$currmon" -a "$currmon" != "PNG" ]; then
	export MONITOR_OVERRIDE=PNG
else
	d.mon start=PNG
fi	
	
d.frame -e
GmGroup::display "root" 
d.mon stop=PNG



#returning old settings
d.mon select=$curr_mon
export GRASS_WIDTH=$old_GRASSW
export GRASS_HEIGHT=$old_GRASSH
export GRASS_BACKGROUNDCOLOR=$old_TRANSPARENT
export GRASS_TRANSPARENT=$old_BACKGROUND
export GRASS_PNG_COMPRESSION=$old_COMPRESSION


echo " "
echo "Screen export complete (writing the file may take a small amount of time)"


if [ "$GIS_OPT_format" != "png" -a  "$GIS_OPT_format" != "ppm" ] ; then
    output2=$GIS_OPT_output"."$GIS_OPT_format
    sleep 5
else 
    exit 1
fi
#size=`ls -s $output | awk '{printf $1}'`

echo " "
echo "******************"
echo "Waiting for file to write"
echo " "

if [ "$GIS_OPT_format" = "tif" ] ; then
    echo "Translating to TIFF format"
    gdal_translate $output $output2 -of GTIFF
    rm $output
fi

if [ "$GIS_OPT_format" = "jpg" ] ; then
    echo "Translating to JPEG format"
    gdal_translate $output $output2 -of JPEG -co QUALITY=$GIS_OPT_quality
    rm $output
fi

if [ "$GIS_OPT_format" = "bmp" ] ; then
    echo "Translating to BMP format"
    gdal_translate $output $output2 -of BMP
    rm $output
fi

    echo "Done"


