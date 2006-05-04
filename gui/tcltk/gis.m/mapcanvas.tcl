##########################################################################
#
# MapCanvas.tcl -TclTk canvas display monitors  and display controls 
#    for GIS Manager: GUI for GRASS 6 
#
# Author: Michael Barton (Arizona State University) - Cedric's version
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


# All of these must be sourced before using mapcanvas.tcl:
# source $gmpath/gmtree.tcl
# source $env(GISBASE)/etc/gtcltk/gmsg.tcl
# source $env(GISBASE)/etc/gtcltk/select.tcl
# source $env(GISBASE)/etc/gui.tcl
# This one is going to be handled by pkgIndex:
source $gmpath/maptool.tcl

namespace eval MapCanvas {
	variable array displayrequest # Indexed by mon, true if it wants to get displayed.
	variable array canmodified # Something's modified the canvas or view, indexed by mon.
	variable array can # mon
	variable array mapcan # mon
	variable array mapframe # mon
	variable array canvas_w # mon
	variable array canvas_h # mon
	variable array map_ind # mon
	variable array coords # mon
	global array mapfile # mon
	global array maskfile # mon
	global array outfile # mon
	global array complist # mon
	global array opclist # mon
	global array masklist # mon
    variable array tree # mon
    variable cmstatus
    variable mapmon

	# Current region and region historys
	# Indexed by mon, history (1 (current) - zoomhistories), part (n, s, e, w, nsres, ewres).
	variable array monitor_zooms
	# Depth of zoom history to keep
	variable zoomhistories
	set zoomhistories 7
}

set initwd 640.0
set initht 480.0
set east 0.0
set north 0.0

#image create photo mapimg.$mon

###############################################################################

# Create window and canvas for display
proc MapCanvas::create { } {
    global gmpath
    global outtext
    global env
    global initwd
    global initht
    global east 
    global north
    global b1east b1north
    global tree_pane
    global mon
    global win
    global currmon
    variable canvas_w
    variable canvas_h
    global drawprog
	global array MapCanvas::msg # mon
	global mapcursor
	global mapfile
	global maskfile
	global outfile
	global complist
	global opclist
	global masklist
	global tmpdir
	global mappid
	
	variable mapmon
    variable mapframe
	variable mapcan
	variable can
	variable coords
	variable map_ind
	
	# Initialize window and map geometry
	
	set canvas_w($mon) $initwd
	set canvas_h($mon) $initht
	set env(GRASS_WIDTH) $initwd
	set env(GRASS_HEIGHT) $initht
	set drawprog 0
	set win ""

	# Zoom to the current region
	MapCanvas::zoom_gregion $mon

	# Create canvas monitor as top level mainframe
	toplevel .mapcan($mon)

    set mapframe($mon) [MainFrame .mapcan($mon).mf \
   		-textvariable MapCanvas::msg($mon) \
   		-progressvar drawprog -progressmax 100 -progresstype incremental]
   		
   	set mf_frame [$mapframe($mon) getframe]

    # toolbar creation
    set map_tb  [$mapframe($mon) addtoolbar]
    MapToolBar::create $map_tb

	# canvas creation
    set can($mon) [canvas $mf_frame.mapcanvas \
	-borderwidth 0 -closeenough 10.0 -relief groove \
        -width $canvas_w($mon) -height $canvas_h($mon) ]
 
    # setting geometry
    place $can($mon) -in $mf_frame -x 0 -y 0 -anchor nw 
	
	pack $can($mon) -fill both -expand yes
 
    # indicator creation	
    set map_ind($mon) [$mapframe($mon) addindicator -textvariable coords($mon) \
    	-width 33 -justify left -padx 5 -bg white]

    pack $mapframe($mon) -fill both -expand yes

	set mapcursor [$can($mon) cget -cursor]

	MapCanvas::coordconv $mon 

	# bindings for display canvas

	set currmon $mon
	
	# set tempfile for ppm output
	set mappid [pid]
	set mapfile($mon) [eval exec "g.tempfile pid=$mappid"]
	set maskfile($mon) $mapfile($mon)
	append mapfile($mon) ".ppm"
	append maskfile($mon) ".pgm"
	
	# set tempfile and tmp directory path for composite output
	set mappid [pid]
	set outfile($mon) [eval exec "g.tempfile pid=$mappid"]
	set tmpdir [file dirname $outfile($mon)]
	set outfile($mon) [file tail $outfile($mon)]
	append outfile($mon) ".ppm"
		
	set complist($mon) ""
	set opclist($mon) ""
	set masklist($mon) ""

	
	# mouse handlers
	# The coordinate transforms should be done per monitor.
	bind $can($mon) <ButtonPress-1> {
		global b1east b1north
		set b1east  [MapCanvas::scrx2mape %x]
		set b1north [MapCanvas::scry2mapn %y]
	}

	# When a monitor gets the keyboard focus
	# switch monitors in the tree if this isn't the selected one
	# I suspect that setting win is unnecessary (where is it used?)
	bind .mapcan($mon) <FocusIn> "
		global mon currmon win
		set win .mapcan($mon)
		set currmon $mon
		if { \$mon != $mon } {
			set mon $mon
			GmTree::switchpage $mon
		} "
		
	# Does this do anything? No globals...
	# I can't remember if bindings happen at global scope.
	bind $can($mon) <Motion> {
		set scrxmov %x
		set scrymov %y
		set eastcoord [eval MapCanvas::scrx2mape %x]
		set northcoord [eval MapCanvas::scry2mapn %y]
		set coords($mon) "$eastcoord $northcoord"
	}

	# window configuration change handler for resizing
	bind $can($mon) <Configure> "MapCanvas::do_resize $mon"

	# bindings for closing map display window
	bind .mapcan($mon) <Destroy> "MapCanvas::cleanup $mon %W"
}


###############################################################################
# map display procedures

# draw map using png driver and open in canvas
proc MapCanvas::drawmap { mon } {
	variable canvas_h
	variable canvas_w
	variable can
	variable canmodified
	variable monitor_zooms

	set w [winfo width $can($mon)]
	set h [winfo height $can($mon)]

	# Make sure canvas_h and canvas_w are correct
	if { $canvas_w($mon) != $w || $canvas_h($mon) != $h } {
		# Flag this as a modified canvas
		set canmodified($mon) 1
		set canvas_w($mon) $w
		set canvas_h($mon) $h
	}

	# Save the current region so that we do not destroy it
	run_panel [list g.region -u save=gism_temp_region --o]
	# Set the region from our zoom settings
	MapCanvas::gregion_zoom $mon

	set mymodified $canmodified($mon)

	if { $mymodified } {
		set canmodified($mon) 0
		# The canvas or view has been modified
		# Redo the map settings to match the canvas
		MapCanvas::mapsettings $mon
	}

	# Render all the layers
	MapCanvas::runprograms $mon $mymodified
	# Composite them and display
	MapCanvas::composite $mon

	# Load the old current region
	run_panel [list g.region region=gism_temp_region --o]
}

# set up map geometry 
proc MapCanvas::mapsettings { mon } {
	global outtext
	global env
	global gmpath
	global mapimg.$mon
	global gisdbase
	global location_name
	global mapset
	variable canvas_h
	variable canvas_w
	global mapdispwd
	global mapdispht
	global mapfile
	
	variable mapcan
	variable can
	variable monitor_zooms
				
	if {[info exists env(MONITOR_OVERRIDE)]} {unset env(MONITOR_OVERRIDE)}

	# get region extents
	set map_n $monitor_zooms($mon,1,n)
	set map_s $monitor_zooms($mon,1,s)
	set map_w $monitor_zooms($mon,1,w)
	set map_e $monitor_zooms($mon,1,e)
		
	set mapwd [expr {abs(1.0 * ($map_e - $map_w))}]
	set mapht [expr {abs(1.0 * ($map_n - $map_s))}]
	
	if { [expr {1.0 * $canvas_h($mon) / $canvas_w($mon)}] > [expr {$mapht / $mapwd}] } {
		set mapdispht [expr {1.0 * $canvas_w($mon) * $mapht / $mapwd}]
		set mapdispwd $canvas_w($mon)
	} else {
		set mapdispht $canvas_h($mon)
		set mapdispwd [expr {1.0 * $canvas_h($mon) * $mapwd / $mapht}]
	}

	# stop display driver in order to set display environment parameters
	if {![catch {open "|d.mon -L" r} input]} {
		while {[gets $input line] >= 0} {
			if {[regexp "^gism.*       running" $line]} {
				runcmd "d.mon stop=gism"
				#wait to make sure that the driver is shut down
				#after 500
				break
			}
		}
		close $input
	}
		
	#set display environment
	set env(GRASS_WIDTH) "$mapdispwd"
	set env(GRASS_HEIGHT) "$mapdispht"
	set env(GRASS_PNGFILE) "$mapfile($mon)"
	set env(GRASS_BACKGROUNDCOLOR) "ffffff"
	set env(GRASS_TRANSPARENT) "TRUE"
	set env(GRASS_PNG_AUTO_WRITE) "TRUE"
	set env(GRASS_TRUECOLOR) "TRUE"
		
	#restart display driver to apply environment settings
	if {![catch {open "|d.mon -L" r} input]} {
		while {[gets $input line] >= 0} {
			if {[regexp "^gism.*       not running" $line]} {
				runcmd "d.mon start=gism -s"
				break
			}
		}
		close $input
	}
}

# Run the programs to clear the map and draw all of the layers
proc MapCanvas::runprograms { mon mod } {
	global outtext
	global env
	global gmpath
	global mapset
	variable canvas_w
	variable canvas_h
	global mapimg.mon
	global mapfile
	global drawprog
	global MapCanvas::msg
	global mapfile
	global drawprog
	global complist
	global masklist
	global opclist
	variable mapframe
	variable mapcan
	variable can
	
	set drawprog 0
	
	# reset compositing list prior to rendering
	set complist($mon) ""
	set opclist($mon) ""
	set masklist($mon) ""
	
    set MapCanvas::msg($mon) "please wait..."
    $mapframe($mon) showstatusbar progression 
		
	incr drawprog
	set env(MONITOR_OVERRIDE) "gism"
	runcmd "d.font romans"
	incr drawprog
	runcmd "d.frame -e"
	incr drawprog
	GmGroup::display "root" $mod
	incr drawprog
	if {[info exists env(MONITOR_OVERRIDE)]} {unset env(MONITOR_OVERRIDE)}
	incr drawprog
}

# composite maps and create canvas
proc MapCanvas::composite {mon } {
	global mapdispwd
	global mapdispht
	global drawprog
	global complist
	global masklist
	global opclist
	global mapfile
	global maskfile
	global outfile
	global tmpdir
	variable mapframe
	variable mapcan
	variable can

	$can($mon) delete all
	if {$complist($mon) != ""} {
		set currdir [pwd]
		cd $tmpdir
		incr drawprog
		runcmd "g.pnmcomp in=$complist($mon) mask=$masklist($mon) opacity=$opclist($mon) background=255:255:255 width=$mapdispwd height=$mapdispht out=$outfile($mon)"
	
		image create photo mapimg.$mon -file "$outfile($mon)" 
		incr drawprog
		$can($mon) create image 0 0 -anchor nw \
			-image "mapimg.$mon" \
			-tag map$mon
		cd $currdir
	}

	GmTree::cvdisplay "root"
	set drawprog 100

	MapCanvas::coordconv $mon
	set drawprog 0
	set MapCanvas::msg($mon) "east & north coordinates under cursor"
	$mapframe($mon) showstatusbar status 
	return

}

###############################################################################
# map display server
# The job of these procedures is to make sure that:
# 1: we are never running more than one update at once.
# 2: we don't do exactly the same update multiple times.

proc MapCanvas::display_server {} {
	variable redrawrequest

	foreach mon [array names redrawrequest] {
		if {$redrawrequest($mon)} {
			# Mark that this monitor no longer wants to be redrawn
			set redrawrequest($mon) 0
			# Redraw the monitor canvas
			MapCanvas::drawmap $mon
		}
	}

	# Do me again in a short period of time.
	# vwait might be appropriate here
	after 100 MapCanvas::display_server
}

# Request a redraw on a monitor
proc MapCanvas::request_redraw {mon modified} {
	variable redrawrequest
	variable canmodified

	set redrawrequest($mon) 1

	if {$modified} {
		set canmodified($mon) 1
	}
}

# Start the server
after idle MapCanvas::display_server

###############################################################################

proc MapCanvas::do_resize {mon} {
	variable canvas_w
	variable canvas_h
	variable can

	# Get the actual width and height of the canvas
	set w [winfo width $can($mon)]
	set h [winfo height $can($mon)]

	# Only actually resize and redraw if the size is different
	if { $canvas_w($mon) != $w || $canvas_h($mon) != $h } {
		$can($mon) delete map$mon
		MapCanvas::request_redraw $mon 1
	}
}


###############################################################################

# erase to white
proc MapCanvas::erase { mon } {
    	
	variable mapcan
	variable can
		
	$can($mon) delete map$mon
	$can($mon) delete all
	
}


###############################################################################

# zoom to current region
proc MapCanvas::zoom_current { mon } {
	variable can
    
	MapCanvas::zoom_gregion $mon
	$can($mon) delete map$mon
	MapCanvas::request_redraw $mon 1
}

###############################################################################

# zoom to default region
proc MapCanvas::zoom_default { mon } {
	variable can
    
	MapCanvas::zoom_gregion $mon [list "-d"]
	$can($mon) delete map$mon
	MapCanvas::request_redraw $mon 1
}

###############################################################################

# zoom to saved region
proc MapCanvas::zoom_region { mon } {
	variable can

	set reg [GSelect windows]
	if { $reg != "" } {
		MapCanvas::zoom_gregion $mon [list "region=$reg"]
		$can($mon) delete map$mon
		MapCanvas::request_redraw $mon 1
	}
}


###############################################################################

# stop display management tools
proc MapCanvas::stoptool { mon } {
	global MapCanvas::msg
	global stop x y east north
    global linex1 liney1 linex2 liney2
    global mlength totmlength
	variable can
	
	if {[$can($mon) find withtag mline] != 0} {
    	$can($mon) delete mline
	}

	if {[$can($mon) find withtag mline] != 0} {
    	$can($mon) delete totmline
	}
	
	# release bindings
	bind $can($mon) <1> ""
	bind $can($mon) <2> ""
	bind $can($mon) <3> ""
	bind $can($mon) <B1-Motion> ""
	bind $can($mon) <ButtonRelease-1> ""

	# reset status display to normal
	set MapCanvas::msg($mon) "east & north coordinates under cursor"

	MapCanvas::restorecursor $mon 		
	
}

###############################################################################
# procedures for interactive zooming in and zooming out

# Zoom or pan to new bounds in the zoom history
# Arguments are either n s w e or n s w e nsres ewres
proc MapCanvas::zoom_new {mon args} {
	variable monitor_zooms
	variable zoomhistories

	set attrs {n s w e nsres ewres}

	# Demote all of the zoom history
	for {set i $zoomhistories} {$i > 1} {incr i -1} {
		set iminus [expr {$i - 1}]
		foreach attr $attrs {
			catch {set monitor_zooms($mon,$i,$attr) $monitor_zooms($mon,$iminus,$attr)}
		}
	}

	# If cols and rows aren't present we just use what was already here.
	set present_attrs [lrange $attrs 0 [expr {[llength $args] - 1}]]

	foreach value $args attr $present_attrs {
		set monitor_zooms($mon,1,$attr) $value
	}
}

# Zoom to the previous thing in the zoom history
proc MapCanvas::zoom_previous {mon} {
	variable monitor_zooms
	variable zoomhistories

	set attrs {n s w e nsres ewres}

	# Remember the first monitor
	set old1 {}
	foreach attr $attrs {
		lappend old1 $monitor_zooms($mon,1,$attr)
	}

	# Promote all of the zoom history
	for {set i 1} {$i < $zoomhistories } {incr i} {
		set iplus [expr {$i + 1}]
		foreach attr $attrs {
			catch {set monitor_zooms($mon,$i,$attr) $monitor_zooms($mon,$iplus,$attr)}
		}
	}

	# Set the oldest thing in the history to where we just were
	foreach value $old1 attr $attrs {
		set monitor_zooms($mon,$zoomhistories,$attr) $value
	}
}


# Zoom to something loaded from a g.region command
proc MapCanvas::zoom_gregion {mon args} {
	if {![catch {open [concat "|g.region" "-ug" $args] r} input]} {
		while {[gets $input line] >= 0} {
			regexp -nocase {^([a-z]+)=(.*)$} $line trash key value
			set parts($key) $value	
		}
		close $input

		MapCanvas::zoom_new $mon $parts(n) $parts(s) $parts(w) $parts(e) $parts(nsres) $parts(ewres)
	}
}


# Set the region from the zoom
proc MapCanvas::gregion_zoom {mon args} {
	variable monitor_zooms

	set attrs {n s w e nsres ewres}

	set options {}
	foreach attr $attrs {
		lappend options "$attr=$monitor_zooms($mon,1,$attr)"
	}

	run_panel [concat g.region $options $args]
}



# zoom bindings
proc MapCanvas::zoombind { mon zoom } {
	variable can
	global mapcursor
	global MapCanvas::msg
    global areaX1 areaY1 areaX2 areaY2
    
    # initialize zoom rectangle corners

	set areaX1 0
	set areaY1 0
	set areaX2 0
	set areaY2 0

	MapCanvas::setcursor $mon "plus"
	
	if {$zoom == 1} {
		set MapCanvas::msg($mon) "Drag or click mouse to zoom"
	} elseif {$zoom == -1} {
		set MapCanvas::msg($mon) "Drag or click mouse to unzoom"
	}

	bind $can($mon) <1> {
		MapCanvas::markzoom $mon %x %y
		}
	bind $can($mon) <B1-Motion> {
		set scrxmov %x
		set scrymov %y
		set eastcoord [eval MapCanvas::scrx2mape %x]
		set northcoord [eval MapCanvas::scry2mapn %y]
		set coords($mon) "$eastcoord $northcoord"
		MapCanvas::drawzoom $mon %x %y
		}
	bind $can($mon) <ButtonRelease-1> "MapCanvas::zoomregion $mon $zoom"

}

# start zoom rectangle
proc MapCanvas::markzoom {mon x y} {
    global areaX1 areaY1 areaX2 areaY2
    variable can

	# initialize corners
	set areaX1 0
	set areaY1 0
	set areaX2 0
	set areaY2 0
    
    set areaX1 [$can($mon) canvasx $x]
    set areaY1 [$can($mon) canvasy $y]
    $can($mon) delete area
}

# draw zoom rectangle
proc MapCanvas::drawzoom { mon x y } {
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
proc MapCanvas::zoomregion { mon zoom } {
	variable can
	variable canvas_h
	variable canvas_w
	variable monitor_zooms
    global areaX1 areaY1 areaX2 areaY2
    
    # if click and no drag, zoom in or out by 80% of original area
    
	if {($areaX2 == 0) && ($areaY2 == 0)} {
		set X2 [expr {$areaX1 + (0.8 * $canvas_w($mon) / 2)} ]
		set X1 [expr {$areaX1 - (0.8 * $canvas_w($mon) / 2)} ]
		set Y2 [expr {$areaY1 + (0.8 * $canvas_h($mon) / 2)} ]
		set Y1 [expr {$areaY1 - (0.8 * $canvas_h($mon) / 2) }]	
		set areaX1 $X1
		set areaY1 $Y1
		set areaX2 $X2
		set areaY2 $Y2
	}
    
	
	# get region extents
	set map_n $monitor_zooms($mon,1,n)
	set map_s $monitor_zooms($mon,1,s)
	set map_w $monitor_zooms($mon,1,w)
	set map_e $monitor_zooms($mon,1,e)
	
	# get zoom rectangle extents in canvas coordinates
	if { $areaX2 < $areaX1 } {
		set cright $areaX1
		set cleft $areaX2
	} else {
		set cleft $areaX1 
		set cright $areaX2
	}
	
	if { $areaY2 < $areaY1 } {
		set cbottom $areaY1
		set ctop $areaY2
	} else {
		set ctop $areaY1 
		set cbottom $areaY2
	}

	# get zoom rectangle extents in map coordinates
	
	set north [scry2mapn $ctop]
	set south [scry2mapn $cbottom]
	set east  [scrx2mape $cright]
	set west  [scrx2mape $cleft]

	# zoom in
	if { $zoom == 1 } {
		MapCanvas::zoom_new $mon $north $south $west $east
	}
	
	#zoom out
	if { $zoom == -1 } {
		set upnorth [expr {$map_n + abs($map_n - $north)}]
		set downsouth [expr {$map_s - abs($south - $map_s)}]
		set backeast  [expr {$map_e + abs($map_e - $east)}]
		set outwest  [expr {$map_w - abs($west - $map_w)}]
		MapCanvas::zoom_new $mon $upnorth $downsouth $outwest $backeast
	}

	# redraw map
	$can($mon) delete map$mon
	$can($mon) delete area
	MapCanvas::request_redraw $mon 1
}



###############################################################################

# zoom back
proc MapCanvas::zoom_back { mon } {
	variable can

	MapCanvas::zoom_previous $mon
	$can($mon) delete map$mon
	MapCanvas::request_redraw $mon 1
}


###############################################################################
#procedures for panning

# pan bindings
proc MapCanvas::panbind { mon } {
	variable can
	global mapcursor
	global MapCanvas::msg

    set MapCanvas::msg($mon) "Drag with mouse to pan"
    	
	MapCanvas::setcursor $mon "hand2"

	bind $can($mon) <1> {MapCanvas::startpan $mon %x %y}
	bind $can($mon) <B1-Motion> {
		set scrxmov %x
		set scrymov %y
		set eastcoord [eval MapCanvas::scrx2mape %x]
		set northcoord [eval MapCanvas::scry2mapn %y]
		set coords($mon) "$eastcoord $northcoord"
		MapCanvas::dragpan $mon %x %y
		}
	bind $can($mon) <ButtonRelease-1> {
		MapCanvas::pan $mon
		}
}


proc MapCanvas::startpan {mon x y} {
    global start_x start_y
    global from_x from_y
    global to_x to_y
	variable can

    set start_x [$can($mon) canvasx $x]
    set start_y [$can($mon) canvasy $y]
	set from_x $start_x
    set from_y $start_y
	set to_x $start_x
    set to_y $start_y

}

proc MapCanvas::dragpan {mon x y} {
    global start_x start_y
    global to_x to_y
	variable can

    set to_x [$can($mon) canvasx $x]
    set to_y [$can($mon) canvasy $y]
    $can($mon) move current [expr {$to_x-$start_x}] [expr {$to_y-$start_y}]
    
    set start_y $to_y
   	set start_x $to_x
}

proc MapCanvas::pan { mon } {
    global from_x from_y
    global to_x to_y
	variable can
	variable monitor_zooms
	
	# get map coordinate shift    
    set from_e [scrx2mape $from_x]
    set from_n [scry2mapn $from_y]
    set to_e   [scrx2mape $to_x]
    set to_n   [scry2mapn $to_y]
    
	# get region extents
	set map_n $monitor_zooms($mon,1,n)
	set map_s $monitor_zooms($mon,1,s)
	set map_w $monitor_zooms($mon,1,w)
	set map_e $monitor_zooms($mon,1,e)

	# set new region extents
	set north [expr {$map_n - ($to_n - $from_n)}]
	set south [expr {$map_s - ($to_n - $from_n)}]
	set east  [expr {$map_e - ($to_e - $from_e)}]
	set west  [expr {$map_w - ($to_e - $from_e)}]
	
	# reset region and redraw map
	MapCanvas::zoom_new $mon $north $south $west $east
	
	$can($mon) delete map$mon
	MapCanvas::request_redraw $mon 1
}

###############################################################################

proc MapCanvas::setcursor { mon  ctype } {
	global mapcursor
	variable can

	$can($mon) configure -cursor $ctype
	return
}

proc MapCanvas::restorecursor {mon} {
	global mapcursor
	variable can
	
	$can($mon) configure -cursor $mapcursor
	return
}

###############################################################################
# procedures for measuring 

# measurement bindings
proc MapCanvas::measurebind { mon } {
	variable can
	variable measurement_annotation_handle
	global mlength totmlength
	global mapcursor
    global linex1 liney1 linex2 liney2
	global MapCanvas::msg

	# Make the output for the measurement
	set measurement_annotation_handle [monitor_annotation_start $mon "Measurement" {}]
    
	if {[info exists linex1]} {unset linex1}
	if {[info exists liney1]} {unset liney1}
	if {[info exists linex2]} {unset linex2}
	if {[info exists liney2]} {unset liney2}
		
	bind $can($mon) <1> "MapCanvas::markmline $mon %x %y"
	bind $can($mon) <B1-Motion> {
		set scrxmov %x
		set scrymov %y
		set eastcoord [eval MapCanvas::scrx2mape %x]
		set northcoord [eval MapCanvas::scry2mapn %y]
		set coords($mon) "$eastcoord $northcoord"
		MapCanvas::drawmline $mon %x %y
		}
	bind $can($mon) <ButtonRelease-1> "MapCanvas::measure $mon"
	
    set MapCanvas::msg($mon) "Draw measure line with mouse"
		
	MapCanvas::setcursor $mon "pencil"
	set mlength 0
	set totmlength 0

}

# start measurement line
proc MapCanvas::markmline {mon x y} {
    global linex1 liney1 linex2 liney2
    variable can
    
    #start line
    if { ![info exists linex1] } {
    	set linex1 [$can($mon) canvasx $x]
    	set liney1 [$can($mon) canvasy $y]
    }

	#check for click with no drag
    if { ![info exists linex2] } {
		set linex2 $linex1
	}
    if { ![info exists liney2] } {
		set liney2 $liney1
	}

    $can($mon) delete mline
}

# draw measurement line
proc MapCanvas::drawmline { mon x y } {
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
proc MapCanvas::measure { mon } {
	variable can
    variable measurement_annotation_handle
	
    # These should all be variables of the canvas, not globals:
    global linex1 liney1 linex2 liney2
    global mlength totmlength
	
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
	set mlength [expr {sqrt(pow(($east1 - $east2), 2) + pow(($north1 - $north2), 2))}]
	set totmlength [expr {$totmlength + $mlength}]
	
	monitor_annotate $measurement_annotation_handle " --segment length\t= $mlength\n"
	monitor_annotate $measurement_annotation_handle "cumulative length\t= $totmlength\n"
	
	set linex1 $linex2
	set liney1 $liney2
}


###############################################################################
# procedures for querying 

# query bindings
proc MapCanvas::querybind { mon } {
	global stop
	global map_ew
	global map_ns	
	global scr_ew
	global scr_ns
	global vdist
	global type
	global options
	global mapname
	global selected
	global mapcursor
	variable tree
	variable can
	
	# set query 'snapping' distance to 10 screen pixels
	set vdist [expr 10* {($map_ew / $scr_ew)} ]
	
    set MapCanvas::msg($mon) "Click to query feature"

	bind $can($mon) <1> {
		MapCanvas::query $mon %x %y 
		}

	MapCanvas::setcursor $mon "crosshair"

}

# query
proc MapCanvas::query { mon x y } {
	global stop
	global vdist
	variable tree
	variable can

	set east  [scrx2mape $x]
	set north [scry2mapn $y]

	# get currently selected map for querying
    set tree($mon) $GmTree::tree($mon)
    
    set sel [ lindex [$tree($mon) selection get] 0 ]

    if { $sel == "" } { return }
    
    set type [GmTree::node_type $sel]

    switch $type {
        "raster" {
            set mapname [GmRaster::mapname $sel]
			set cmd "r.what -f input=$mapname east_north=$east,$north\n\n"
        }
        "vector" {
            set mapname [GmVector::mapname $sel]
	    	set cmd "v.what -a map=$mapname east_north=$east,$north distance=$vdist\n\n"
        }
        "rgbhis" {
            set mapname [GmRgbhis::mapname $sel]
			set cmd "r.what -f input=$mapname east_north=$east,$north\n\n"
        }
        dframe {
            return
        }
        chart {
            return
        }
        thematic {
            return
        }
    }

	if { $mapname == "" } {
		set ah [monitor_annotation_start $mon "Query" {}]
		monitor_annotate $ah "You must select a map to query\n"
		return
	}
	
	run_panel $cmd
}

###############################################################################

# print to eps file
proc MapCanvas::printcanvas { mon } {
	variable mapcan
	variable can
	variable canvas_w
	variable canvas_h
	
	set cv $can($mon)
	
	# open print window
	psprint::init
    psprint::window $mon $cv $canvas_w($mon) $canvas_h($mon)
}

###############################################################################

#	Set up initial variables for screen to map conversion
proc MapCanvas::coordconv { mon } {

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
	variable mapframe
	variable canvas_w
	variable canvas_h
	variable monitor_zooms

	# get current region extents
	set map_n $monitor_zooms($mon,1,n)
	set map_s $monitor_zooms($mon,1,s)
	set map_w $monitor_zooms($mon,1,w)
	set map_e $monitor_zooms($mon,1,e)
	
# 	calculate dimensions

	set map_n [expr {1.0*($map_n)}]
	set map_s [expr {1.0*($map_s)}]
	set map_e [expr {1.0*($map_e)}]
	set map_w [expr {1.0*($map_w)}]
	
	set map_ew [expr {$map_e - $map_w}]
	set map_ns [expr {$map_n - $map_s}]


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

	set map2scrx_conv [expr {$scr_ew / $map_ew}]
	set map2scry_conv [expr {$scr_ns / $map_ns}]
		
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
proc MapCanvas::mapn2scry { north } {
	global map_n
	global scr_n
	global map2scry_conv

	return [expr {$scr_n + (($map_n - $north) * $map2scry_conv)}]
}

# map east to screen x
proc MapCanvas::mape2scrx { east } {
	global map_w
	global scr_w
	global map2scrx_conv

	return [expr {$scr_w + (($east - $map_w) * $map2scrx_conv)}]

}

# screen y to map north
proc MapCanvas::scry2mapn { y } {
	global map_n
	global scr_n
	global map2scry_conv

	return [expr {$map_n - (($y - $scr_n) / $map2scry_conv)}]

}

# screen x to map east
proc MapCanvas::scrx2mape { x } {
	global map_w
	global scr_w
	global map2scrx_conv

	return [expr {$map_w + (($x - $scr_w) / $map2scrx_conv)}]

}

###############################################################################
# pass mapcan parameter
proc MapCanvas::getmapcan { mon } {
	variable mapcan
	
	set mc $mapcan($mon)
	return $mc
}

###############################################################################
# cleanup procedure on closing window
proc MapCanvas::cleanup { mon destroywin} {
	global pgs

	if { $destroywin == ".mapcan($mon)" } { 
		$pgs delete "page_$mon"
		# runcmd "g.mremove -f region=mon_$mon "
		if { [winfo exists .tlegend($mon)] } { destroy .tlegend($mon) }
	}

	# stop gism PNG driver if it is still running due to error
	if {![catch {open "|d.mon -L" r} input]} {
		while {[gets $input line] >= 0} {
			if {[regexp {^gism            Create PNG Map for gism        running} $line]} {
				runcmd "d.mon stop=gism"
				break
			}
		}
		close $input
	}
	
}

###############################################################################
