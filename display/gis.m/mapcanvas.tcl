##########################################################################
#
# MapCanvas.tcl
#
# TclTk canvas display monitors  and display controls 
#    for GIS Manager: GUI for GRASS 6 
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

source $gmpath/maptool.tcl
source $gmpath/gmtree.tcl
source $env(GISBASE)/etc/gtcltk/gmsg.tcl
source $env(GISBASE)/etc/gtcltk/select.tcl
source $env(GISBASE)/etc/gui.tcl

set bgcolor HoneyDew2

namespace eval mapcan {
	variable array can # mon
	variable array mapcan # mon
	variable array canvas_w # mon
	variable array canvas_h # mon
	variable array canvas_w # mon
	variable array canvas_h # mon
    variable cmstatus
	}

set initwd 640
set initht 480
set east 0
set north 0
image create photo mapimg.$mon

###############################################################################

# Create window and canvas for display
proc mapcan::create { } {
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
    global currmon

	variable mapcan
	variable can
	variable canvas_w
	variable canvas_h
	variable canvas_w
	variable canvas_h

#	variable tree
	
	# Initialize window and map geometry
	
	set canvas_w($mon) $initwd
	set canvas_h($mon) $initht
	set env(GRASS_WIDTH) $initwd
	set env(GRASS_HEIGHT) $initht
	
	set win ""


	# Create canvas monitor as top level mainframe
	toplevel .mapcan($mon)

    set mapframe [MainFrame .mapcan($mon).mapframe \
   		-background $bgcolor -textvariable mapcan::status ]


    # toolbar creation
    set map_tb  [$mapframe addtoolbar]
    MapToolBar::create $map_tb

    
	# canvas creation
    set can($mon) [canvas $mapframe.can \
        -background #ffffff -borderwidth 0 -closeenough 1.0 \
        -insertbackground black -relief ridge -selectbackground #c4c4c4 \
        -selectforeground black -width $canvas_w($mon) -height $canvas_h($mon) ]
    
    # setting geometry
    place $mapframe.can \
        -in $mapframe -x 0 -y 0 -anchor nw \
        -bordermode ignore 
	
    set mapcan::status \
    	"geographic coordinates under cursor (east north)"
    $mapframe showstatusbar $mapcan::status 

    pack $mapframe -expand yes -fill both -ipadx 0 -ipady 0
	pack $map_tb -expand no -fill x -anchor nw -side bottom
	pack $mapframe.can -fill both -expand yes -anchor n -side top	
 
    set fon [font create -family Verdana -size 12 ]
    DynamicHelp::configure -font $fon -background yellow

	mapcan::coordconv $mon 

	# bindings for display canvas

	set currmon $mon
	
	# mouse handlers
	bind $can($mon) <ButtonPress-1> {
		global  mon b1east b1north win
		variable tree		
		set winx [winfo pointerx .]
		set winy [winfo pointery .]
		set win [winfo containing $winx $winy]
		regexp -nocase {.*\((\d*)(\).*)} $win win1 currmon win2
		set mon $currmon
		set b1east  [mapcan::scrx2mape %x]
		set b1north [mapcan::scry2mapn %y]
		GmTree::switchpage $mon
	}
	
	bind $mapframe <ButtonPress-1> {
		global  mon b1east b1north win
		variable tree		
		set winx [winfo pointerx .]
		set winy [winfo pointery .]
		set win [winfo containing $winx $winy]
		regexp -nocase {.*\((\d*)(\).*)} $win win1 currmon win2
		set mon $currmon
		GmTree::switchpage $mon
	}
	bind .mapcan($mon) <ButtonPress-1> {
		global  mon b1east b1north win
		variable tree		
		set winx [winfo pointerx .]
		set winy [winfo pointery .]
		set win [winfo containing $winx $winy]
		regexp -nocase {.*\((\d*)(\).*)} $win win1 currmon win2
		set mon $currmon
		GmTree::switchpage $mon
	}

	bind $can($mon) <Motion> {
		set scrxmov %x
		set scrymov %y
		set eastcoord [eval mapcan::scrx2mape %x]
		set northcoord [eval mapcan::scry2mapn %y]
		set coords "$eastcoord $northcoord"
	}


    # indicator creation	
    set map_ind  [$mapframe addindicator -textvariable coords \
    	-width 25 -justify left -padx 15]
    	

#    update idletasks    


#	window configuration change handler for resizing
    bind $can($mon) <Configure> {
    	variable canvas_w
    	variable canvas_h
    	#global currmon
		set canvas_w($mon) %w
		set scrwidth %w
		set canvas_h($mon) %h
		set order %d
		regexp -nocase {.*\((\d*)(\).*)} $win win1 currmon win2
		set mon $currmon
		#if { $order == "below" } { set leavemon $changemon }
		after cancel mapcan::do_resize $mon
		after idle mapcan::do_resize $mon
	}
    bind $mapframe <Configure> {
    	variable canvas_w
    	variable canvas_h
    	#global currmon
		set canvas_w($mon) %w
		set scrwidth %w
		set canvas_h($mon) %h
		set order %d
		regexp -nocase {.*\((\d*)(\).*)} $win win1 currmon win2
		set mon $currmon
	}
    bind .mapcan($mon) <Configure> {
    	variable canvas_w
    	variable canvas_h
    	#global currmon
		set canvas_w($mon) %w
		set scrwidth %w
		set canvas_h($mon) %h
		set order %d
		regexp -nocase {.*\((\d*)(\).*)} $win win1 currmon win2
		set mon $currmon
	}
	
}

###############################################################################
# procedures for linking layer tree to display

# show tree for display
proc mapcan::showtree { mon } {
	global tree
	global sw

#	$tree($mon) opentree "root"
    $sw setwidget $tree($mon)

}

# hide tree for display
proc mapcan::hidetree { mon } {
	variable tree
	
#	$tree($mon) closetree "root"
}

###############################################################################
# coordinate conversions for moving mouse pointer

proc mapcan::coordmov { mon } {
	global winxmov winymov coords
	global scrxmov scrymov
	variable mapcan
	variable can
	
	set eastcoord [eval mapcan::scrx2mape $scrxmov]
	set northcoord [eval mapcan::scry2mapn $scrymov]
	set coords "$eastcoord $northcoord"
}


###############################################################################
# map display procedures

# set up map geometry 
proc mapcan::mapsettings { mon } {
	global outtext
	global env
	global gmpath
	global mapimg.$mon
	global gisdbase
	global location_name
	global mapset
	
	variable mapcan
	variable can
	variable canvas_w
	variable canvas_h
			
    set monregion "$gisdbase/$location_name/$mapset/windows/mon_$mon"
	if {[file exists $monregion] } {
		set cmd "g.region region=mon_$mon"	
		runcmd $cmd
	} else {
		set cmd "g.region save=mon_$mon --o"	
		runcmd $cmd
	}
		
	if ![catch {open "|g.region -g" r} input] {
		while {[gets $input line] >= 0} {
			regexp -nocase {n=(.*)} $line n1 map_n
			regexp -nocase {^s=(.*)} $line s1 map_s
			regexp -nocase {e=(.*)} $line e1 map_e
			regexp -nocase {w=(.*)} $line w1 map_w
		}
	}
	
	set mapwd [expr abs(1.0 * ($map_e - $map_w))]
	set mapht [expr abs(1.0 * ($map_n - $map_s))]
	
	if { [expr $canvas_h($mon) / $canvas_w($mon)] > [expr $mapht / $mapwd] } {
		set mapdispht [expr 1.0 * $canvas_w($mon) * $mapht / $mapwd]
		set mapdispwd $canvas_w($mon)
	} else {
		set mapdispht $canvas_h($mon)
		set mapdispwd [expr 1.0 * $canvas_h($mon) * $mapwd / $mapht]
	}

	set env(GRASS_WIDTH) $mapdispwd
	set env(GRASS_HEIGHT) $mapdispht
	set env(GRASS_PNGFILE) "dispmon_$mon.ppm"
	set env(GRASS_TRANSPARENT) "TRUE"
	set env(GRASS_PNG_AUTO_WRITE) "TRUE"
	set env(GRASS_TRUECOLOR) "TRUE"
	
}

# draw map using png driver and open in canvas
proc mapcan::drawmap { mon } {
	global outtext
	global env
	global gmpath
	global mapimg.$mon
	global mapset
	
	variable mapcan
	variable can
	variable canvas_w
	variable canvas_h
		
	$outtext delete 1.0 end
	
    if ![catch {open "|d.mon -L" r} input] {
        while {[gets $input line] >= 0} {
			if {[regexp "^PNG.*not running" $line]} {
				run "d.mon start=PNG"
			} elseif {[regexp "^PNG.* running" $line]} {
				set env(MONITOR_OVERRIDE) "PNG"
			}
		}
	}

 	runcmd "d.frame -e" 
    GmGroup::display "root" 
    runcmd "d.mon stop=PNG" 
	
	$can($mon) create image 0 0 -anchor nw \
		-image [image create photo mapimg.$mon -file "dispmon_$mon.ppm" ] \
		-tag map$mon
	mapcan::coordconv $mon
	
	return
}

###############################################################################

proc mapcan::do_resize {mon} {
	
	variable canvas_w
	variable canvas_h
	variable can
	variable canvas_w
	variable canvas_h

		
	set canvas_w($mon) $canvas_w($mon)
	set canvas_h($mon) $canvas_h($mon)
	
	update idletasks
	mapcan::coordconv $mon
	$can($mon) delete map$mon
	mapcan::mapsettings $mon
	mapcan::drawmap $mon
}


###############################################################################

# erase to white
proc mapcan::erase { mon } {
    	
	variable mapcan
	variable can
		
	$can($mon) delete map$mon
}


###############################################################################

# display all
proc mapcan::zoom_default { mon } {
	variable can
    
	run "g.region save=previous_zoom --o"
	set cmd "g.region -d save=mon_$mon --o"
    run_panel $cmd 
	
	$can($mon) delete map$mon
	mapcan::mapsettings $mon
    mapcan::drawmap $mon
}

###############################################################################

# display region
proc mapcan::zoom_region { mon } {
   	variable can
   
    set reg [GSelect windows]
    if { $reg != "" } {
		run "g.region save=previous_zoom --o"
		set cmd "g.region region=$reg save=mon_$mon --o"
		run_panel $cmd 
    }
	$can($mon) delete map$mon
	mapcan::mapsettings $mon
    mapcan::drawmap $mon
}

###############################################################################
# procedures for zooming in and zooming out

# zoom bindings
proc mapcan::zoombind { mon zoom } {
	variable can

	bind $can($mon) <2> ""
	bind $can($mon) <3> ""
	
	bind $can($mon) <1> "mapcan::markzoom $mon %x %y"
	bind $can($mon) <B1-Motion> "mapcan::drawzoom $mon %x %y"
	bind $can($mon) <ButtonRelease-1> "mapcan::zoomregion $mon $zoom"

}

# start zoom rectangle
proc mapcan::markzoom {mon x y} {
    global areaX1 areaY1
    variable can
    
    set areaX1 [$can($mon) canvasx $x]
    set areaY1 [$can($mon) canvasy $y]
    $can($mon) delete area
}

# draw zoom rectangle
proc mapcan::drawzoom { mon x y } {
	variable can
	
    global areaX1 areaY1 areaX2 areaY2
	    
	set xc [$can($mon) canvasx $x]
	set yc [$can($mon) canvasy $y]
	
	if {($areaX1 != $xc) && ($areaY1 != $yc)} {
		$can($mon) delete area
		$can($mon) addtag area withtag \
			[$can($mon) create rect $areaX1 $areaY1 $xc $yc \
			-outline yellow -width 2]
		set areaX2 $xc
		set areaY2 $yc
	}
}	

# zoom region
proc mapcan::zoomregion { mon zoom } {
	variable can
	
    global areaX1 areaY1 areaX2 areaY2
	
	# get region extents
	if ![catch {open "|g.region -g" r} input] {
		while {[gets $input line] >= 0} {
			regexp -nocase {n=(.*)} $line n1 map_n
			regexp -nocase {^s=(.*)} $line s1 map_s
			regexp -nocase {e=(.*)} $line e1 map_e
			regexp -nocase {w=(.*)} $line w1 map_w
		}
	}

	# get zoom rectangle extents in canvas coordinates
	if { $areaX2 > $areaX1 } {
		set cleft $areaX1 
		set cright $areaX2
	} else {
		set cright $areaX1
		set cleft $areaX2
	}
	
	if { $areaY2 > $areaY1 } {
		set ctop $areaY1 
		set cbottom $areaY2
	} else {
		set cbottom $areaY1
		set ctop $areaY2
	}

	# get zoom rectangle extents in map coordinates
	
	set north [scry2mapn $ctop]
	set south [scry2mapn $cbottom]
	set east  [scrx2mape $cright]
	set west  [scrx2mape $cleft]

	# zoom in
	if { $zoom == 1 } {
		run "g.region save=previous_zoom --o"
		set cmd "g.region n=$north s=$south \
			e=$east w=$west save=mon_$mon --o"
		run $cmd
	}
	
	#zoom out
	if { $zoom == -1 } {
		set upnorth [expr $map_n + abs($map_n - $north)]
		set downsouth [expr $map_s - abs($south - $map_s)]
		set backeast  [expr $map_e + abs($map_e - $east)]
		set outwest  [expr $map_w - abs($west - $map_w)]
		run "g.region save=previous_zoom --o"
		set cmd "g.region n=$upnorth s=$downsouth \
			e=$backeast w=$outwest save=mon_$mon --o"
		run $cmd
	}

	# redraw map
	$can($mon) delete map$mon
    $can($mon) delete area
	mapcan::mapsettings $mon
	mapcan::drawmap $mon
	
	# release bindings
	bind $can($mon) <1> ""
	bind $can($mon) <B1-Motion> ""
	bind $can($mon) <ButtonRelease-1> ""

	return
}

# reinitialize zoom rectangle corners

set areaX1 0
set areaY1 0
set areaX2 0
set areaY2 0



###############################################################################

# zoom back
proc mapcan::zoom_back { mon } {
    variable can
    
    set cmd "g.region region=previous_zoom save=mon_$mon --o"
    runcmd $cmd
	$can($mon) delete map$mon
	mapcan::mapsettings $mon
	mapcan::drawmap $mon

}


###############################################################################
#procedures for panning

# pan bindings
proc mapcan::panbind { mon } {
	variable can
	
	bind $can($mon) <2> ""
	bind $can($mon) <3> ""

	bind $can($mon) <1> "mapcan::startpan $mon %x %y"
	bind $can($mon) <B1-Motion> "mapcan::dragpan $mon %x %y"
	bind $can($mon) <ButtonRelease-1> "mapcan::pan $mon"
}


proc mapcan::startpan {mon x y} {
    global start_x start_y
    global from_x from_y
	variable can

    set start_x [$can($mon) canvasx $x]
    set start_y [$can($mon) canvasy $y]
	set from_x $start_x
    set from_y $start_y

}

proc mapcan::dragpan {mon x y} {
    global start_x start_y
    global to_x to_y
	variable can

    set to_x [$can($mon) canvasx $x]
    set to_y [$can($mon) canvasy $y]
    $can($mon) move current [expr {$to_x-$start_x}] [expr {$to_y-$start_y}]
    
    set start_y $to_y
   	set start_x $to_x
}

proc mapcan::pan { mon } {
    global from_x from_y
    global to_x to_y
	variable can
	
	# get map coordinate shift    
    set from_e [scrx2mape $from_x]
    set from_n [scry2mapn $from_y]
    set to_e   [scrx2mape $to_x]
    set to_n   [scry2mapn $to_y]
    
	# get region extents
	if ![catch {open "|g.region -g" r} input] {
		while {[gets $input line] >= 0} {
			regexp -nocase {n=(.*)} $line n1 map_n
			regexp -nocase {^s=(.*)} $line s1 map_s
			regexp -nocase {e=(.*)} $line e1 map_e
			regexp -nocase {w=(.*)} $line w1 map_w
		}
	}

	# set new region extents
	set north [expr $map_n - ($to_n - $from_n)]
	set south [expr $map_s - ($to_n - $from_n)]
	set east  [expr $map_e - ($to_e - $from_e)]
	set west  [expr $map_w - ($to_e - $from_e)]
	
	# reset region and redraw map
	run "g.region save=previous_zoom --o"
	set cmd "g.region n=$north s=$south \
		e=$east w=$west save=mon_$mon --o"
	run $cmd
	
	$can($mon) delete map$mon
	mapcan::mapsettings $mon
	mapcan::drawmap $mon    
	
	# unbind events
	bind $can($mon) <1> ""
	bind $can($mon) <B1-Motion> ""
	bind $can($mon) <ButtonRelease-1> "" 

    return
}

###############################################################################
# procedures for measuring 

# measurement bindings
proc mapcan::measurebind { mon } {
	variable can
	global mlength totmlength dtxt

	bind $can($mon) <2> ""
	
	bind $can($mon) <1> "mapcan::markmline $mon %x %y"
	bind $can($mon) <B1-Motion> "mapcan::drawmline $mon %x %y"
	bind $can($mon) <ButtonRelease-1> "mapcan::measure $mon"
	bind $can($mon) <3> "mapcan::stopmeasure $mon"
	
	if { ![winfo exists .dispout]} {Gm::create_disptxt $mon}
	
	$dtxt insert end "Use mouse (L button) to draw measurement line\n"
	$dtxt insert end "Press right mouse button to end measurement\n\n"
	$dtxt yview end 
	catch {cmd_output $fh}
	
	set mlength 0
	set totmlength 0

}

# start measurement line
proc mapcan::markmline {mon x y} {
    global linex1 liney1 linex2 liney2
    variable can
    
    # create window for measurement output
    # put some code here
    
    #start line
    if { ![info exists linex1] } {
    	set linex1 [$can($mon) canvasx $x]
    	set liney1 [$can($mon) canvasy $y]
    }
    
    $can($mon) delete mline
}

# draw measurement line
proc mapcan::drawmline { mon x y } {
	variable can
	
    global linex1 liney1 linex2 liney2
	    
	set xc [$can($mon) canvasx $x]
	set yc [$can($mon) canvasy $y]
	
	# draw line segment
	if {($linex1 != $xc) && ($liney1 != $yc)} {
		$can($mon) delete mline
		$can($mon) addtag mline withtag \
			[$can($mon) create line $linex1 $liney1 $xc $yc \
			-fill red -arrow both -width 2]
		set linex2 $xc
		set liney2 $yc
	}
}	

# measure line length
proc mapcan::measure { mon } {
	variable can
	
    global linex1 liney1 linex2 liney2
    global mlength totmlength
    global dtxt
	
	# draw cumulative line
	$can($mon) addtag totmline withtag \
		[$can($mon) create line $linex1 $liney1 $linex2 $liney2 \
		-fill green -arrow both -width 2]

	# get line endpoints in map coordinates
	
	set east1  [scrx2mape $linex1]
	set north1 [scry2mapn $liney1]
	set east2  [scrx2mape $linex2]
	set north2 [scry2mapn $liney2]

	# calculate line segment length and total length
	set mlength [expr sqrt(pow(($east1 - $east2), 2) + pow(($north1 - $north2), 2))]
	set totmlength [expr $totmlength + $mlength]
	
	$dtxt insert end " --segment length\t= $mlength\n"
	$dtxt insert end "cumulative length\t= $totmlength\n"
	$dtxt yview end 
	catch {cmd_output $fh}
	
	set linex1 $linex2
	set liney1 $liney2


}

# end measurement
proc mapcan::stopmeasure { mon } {
	variable can
	
    global linex1 liney1 linex2 liney2
    global mlength totmlength

	# delete measurement line
    $can($mon) delete mline
    $can($mon) delete totmline
    
	unset linex1 
	unset liney1
	unset linex2
	unset liney2

    #destroy measurement window
	#put some code here
	
	
	# release bindings
	bind $can($mon) <1> ""
	bind $can($mon) <2> ""
	bind $can($mon) <B1-Motion> ""
	bind $can($mon) <ButtonRelease-1> ""
	bind $can($mon) <3> ""

	return
}




###############################################################################
# procedures for querying 

# query bindings
proc mapcan::querybind { mon } {
	variable can
	global dtxt
	global stop
	global map_ew
	global map_ns	
	global scr_ew
	global scr_ns
	global vdist


	# set query 'snapping' distance to 10 screen pixels
	set vdist [expr $map_ew / (10* $scr_ew) ]
	
	bind $can($mon) <1> {mapcan::startquery $mon %x %y}
	bind $can($mon) <3> {mapcan::stopquery $mon}
	
	if { ![winfo exists .dispout]} {Gm::create_disptxt $mon}

	$dtxt insert end "Use mouse (L button) to query features\n"
	$dtxt insert end "Press right mouse button to stop query session\n\n"
	$dtxt yview end 
	catch {cmd_output $fh}
	
}

# query
proc mapcan::startquery { mon x y } {
	global stop

	set east  [scrx2mape $x]
	set north [scry2mapn $y]
	
	set stop 0
	GmTree::query $mon $east $north
	if { $stop == 1 } { return }
}

# query
proc mapcan::stopquery { mon } {
	global stop x y east north
	variable can
	
	set stop 1
	update

	# release bindings
	bind $can($mon) <1> ""
	bind $can($mon) <3> ""

	return stop
	
}
###############################################################################

# print to eps file
proc mapcan::printcanvas { mon } {
	variable mapcan
	variable can
		
	$can($mon) postscript -file "map$mon.eps"

}

###############################################################################

#	Set up initial variables for screen to map conversion
proc mapcan::coordconv { mon } {

	global map_n
	global map_s
	global map_e
	global map_w
	global map_ew
	global map_ns	
	global scr_n
	global scr_s
	global scr_e
	global scr_w
	global scr_ew
	global scr_ns
	global map2scrx_conv
	global map2scry_conv
	global mapframe.can
	global mapimg.$mon
	
	variable can
	variable canvas_w
	variable canvas_h
	

#	get current map coordinates from g.region

	if ![catch {open "|g.region -g" r} input] {
		while {[gets $input line] >= 0} {
			regexp -nocase {n=(.*)} $line n1 map_n
			regexp -nocase {^s=(.*)} $line s1 map_s
			regexp -nocase {e=(.*)} $line e1 map_e
			regexp -nocase {w=(.*)} $line w1 map_w
		}
	}

# 	calculate dimensions

	set map_n [expr 1.0*($map_n)]
	set map_s [expr 1.0*($map_s)]
	set map_e [expr 1.0*($map_e)]
	set map_w [expr 1.0*($map_w)]
	
	set map_ew [expr $map_e - $map_w]
	set map_ns [expr $map_n - $map_s]


#	get current screen geometry
if { [info exists "mapimg.$mon"] } {
	set scr_ew [image width "mapimg.$mon"]
	set scr_ns [image height "mapimg.$mon"]
	set scr_e [image width "mapimg.$mon"]
	set scr_s [image height "mapimg.$mon"]
}	else {
	set scr_ew $canvas_w($mon)
	set scr_ns $canvas_h($mon)
	set scr_e $canvas_w($mon)
	set scr_s $canvas_h($mon)
}

	set scr_n 0.0
	set scr_w 0.0

	
# 	calculate conversion factors. Note screen is from L->R, T->B but map
# 	is from L->R, B->T

	set map2scrx_conv [expr $scr_ew / $map_ew]
	set map2scry_conv [expr $scr_ns / $map_ns]
		
# 	calculate screen dimensions and offsets

	if { $map2scrx_conv > $map2scry_conv } {
		set map2scrx_conv $map2scry_conv
	} else {
		set map2scry_conv $map2scrx_conv
	}

}

###############################################################################


# screen to map and map to screen conversion procedures

# map north to screen y
proc mapcan::mapn2scry { north } {
	global map_n
	global scr_n
	global map2scry_conv

	return [expr $scr_n + (($map_n - $north) * $map2scry_conv)]
}

# map east to screen x
proc mapcan::mape2scrx { east } {
	global map_w
	global scr_w
	global map2scrx_conv

	return [expr $scr_w + (($east - $map_w) * $map2scrx_conv)]

}

# screen y to map north
proc mapcan::scry2mapn { y } {
	global map_n
	global scr_n
	global map2scry_conv

	return [expr $map_n - (($y - $scr_n) / $map2scry_conv)]

}

# screen x to map east
proc mapcan::scrx2mape { x } {
	global map_w
	global scr_w
	global map2scrx_conv

	return [expr $map_w + (($x - $scr_w) / $map2scrx_conv)]

}

###############################################################################
# transform window x to canvas x
proc winx2canx { x } {
	global mon
	variable can
	
	return [$can($mon) canvasx x]
}



###############################################################################
# cleanup procedure on closing window
proc mapcan::cleanup { mon } {
	global pgs
	
	$pgs delete "page_$mon"
	runcmd "g.mremove -f region=mon_$mon >/dev/null"
	destroy mon
}

###############################################################################
	


	wm geom . [wm geom .]



